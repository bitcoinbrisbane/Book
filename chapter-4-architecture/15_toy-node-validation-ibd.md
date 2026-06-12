## Toy node — Validation & IBD

This is where the layers come together. We've discovered peers, we can talk to one, we can persist what we receive. Now we need to *trust* the data — validate it before storing — and we need to drive the whole pipeline forward block-by-block from genesis. This page walks through the validation primitives in `Chain.cs`, then the Initial Block Download orchestration in `Program.cs`.

## Block headers and the proof-of-work target

A Bitcoin block header is exactly 80 bytes. Parsing it is mechanical:

```csharp
public sealed class BlockHeader
{
    public int Version { get; init; }
    public byte[] PrevBlock { get; init; } = new byte[32];
    public byte[] MerkleRoot { get; init; } = new byte[32];
    public uint Timestamp { get; init; }
    public uint Bits { get; init; }
    public uint Nonce { get; init; }
    public byte[] Raw { get; init; } = new byte[80];

    public byte[] Hash => Crypto.DoubleSha256(Raw);
    // …
}
```

The `Bits` field is a compact 4-byte representation of a 256-bit *target*. A block is valid only if its `dsha256` is numerically *below* the target. Decompressing the target is a one-liner once you know the trick: top byte is an exponent, low three bytes are a mantissa, and you shift.

```csharp
public BigInteger Target
{
    get
    {
        uint exponent = Bits >> 24;
        uint mantissa = Bits & 0x007fffff;
        if (exponent <= 3) return new BigInteger(mantissa) >> (int)(8 * (3 - exponent));
        return new BigInteger(mantissa) << (int)(8 * (exponent - 3));
    }
}
```

The proof-of-work check is then a single comparison:

```csharp
public static bool CheckProofOfWork(BlockHeader header)
{
    var hashAsNumber = new BigInteger(header.Hash, isUnsigned: true, isBigEndian: false);
    return hashAsNumber <= header.Target;
}
```

Bitcoin stores hashes little-endian internally, which is why we pass `isBigEndian: false`. The familiar "leading zeroes" you see when a block hash is displayed (e.g. `00000000839a8e6886ab…`) are the *trailing* bytes of the on-wire hash, reversed for human eyes.

## Merkle root verification

Every block header commits to its transactions via a merkle root — a single hash that depends on every txid in the block. We recompute it and compare:

```csharp
public byte[] ComputeMerkleRoot()
{
    var hashes = Transactions.Select(t => t.Txid).ToList();
    if (hashes.Count == 0) return new byte[32];
    while (hashes.Count > 1)
    {
        if (hashes.Count % 2 == 1) hashes.Add(hashes[^1]); // duplicate odd tail
        var next = new List<byte[]>(hashes.Count / 2);
        for (int i = 0; i < hashes.Count; i += 2)
        {
            var combined = new byte[64];
            hashes[i].CopyTo(combined, 0);
            hashes[i + 1].CopyTo(combined, 32);
            next.Add(Crypto.DoubleSha256(combined));
        }
        hashes = next;
    }
    return hashes[0];
}
```

The "duplicate odd tail" line is the source of [CVE-2012-2459](https://en.bitcoin.it/wiki/CVEs#CVE-2012-2459) — an attacker can craft two different transaction lists that produce the same merkle root by exploiting it. Modern consensus rejects such blocks at parse time. Our toy doesn't, but we also don't accept blocks from anyone other than mainnet peers, so the practical risk is nil.

## Transaction parsing — the SegWit txid trick

Computing a transaction's txid is *almost* "double-SHA-256 the bytes you received." The exception is SegWit: the witness data inside a segwit tx is *not* part of the txid. So we have to recognise a witness tx and serialise it without the witness fields before hashing:

```csharp
public static Transaction Parse(ReadOnlySpan<byte> bytes, out int consumed)
{
    int p = 0;
    int versionStart = p;
    p += 4;

    bool isWitness = bytes[p] == 0x00 && bytes[p + 1] == 0x01;
    if (isWitness) p += 2;

    int inputsStart = p;
    p += Varint.Read(bytes[p..], out var txInCount);
    for (ulong i = 0; i < txInCount; i++)
    {
        p += 36; // prev_hash (32) + prev_index (4)
        p += Varint.Read(bytes[p..], out var scriptLen);
        p += (int)scriptLen;
        p += 4;  // sequence
    }
    p += Varint.Read(bytes[p..], out var txOutCount);
    for (ulong i = 0; i < txOutCount; i++)
    {
        p += 8;  // value
        p += Varint.Read(bytes[p..], out var scriptLen);
        p += (int)scriptLen;
    }
    int inputsEnd = p;

    if (isWitness)
    {
        for (ulong i = 0; i < txInCount; i++)
        {
            p += Varint.Read(bytes[p..], out var stackItemCount);
            for (ulong j = 0; j < stackItemCount; j++)
            {
                p += Varint.Read(bytes[p..], out var itemLen);
                p += (int)itemLen;
            }
        }
    }

    int locktimeStart = p;
    p += 4;
    consumed = p;

    byte[] forHash;
    if (isWitness)
    {
        int inputsLen = inputsEnd - inputsStart;
        forHash = new byte[4 + inputsLen + 4];
        bytes.Slice(versionStart, 4).CopyTo(forHash);
        bytes.Slice(inputsStart, inputsLen).CopyTo(forHash.AsSpan(4));
        bytes.Slice(locktimeStart, 4).CopyTo(forHash.AsSpan(4 + inputsLen));
    }
    else
    {
        forHash = bytes[..p].ToArray();
    }

    return new Transaction { Txid = Crypto.DoubleSha256(forHash), Size = p };
}
```

The marker/flag pair `0x00 0x01` between version and the input count is what tells us a tx is segwit-encoded. We skip those, parse inputs and outputs identically to a legacy tx, then *also* skip the witness data at the end. For the txid we hash version + inputs/outputs + locktime — never the marker, flag, or witnesses.

This is the smallest possible thing you can do to merkle-verify a modern block. It's not enough to validate the *witness* commitment (the segwit "wtxid" merkle, which lives in the coinbase's last output); for that you'd compute a second merkle tree over wtxids, which the toy skips.

## The IBD loop — header sync

Header sync is a `getheaders` round-trip in a loop. Each batch maxes out at 2000 headers, so to walk further we re-send `getheaders` using the latest hash as our new locator.

```csharp
private static async Task<List<BlockHeader>> DownloadHeaders(
    PeerSession session, IBlockStore store, int max, CancellationToken ct)
{
    var all = new List<BlockHeader>();
    var locator = GenesisHashLE;
    byte[]? prevHash = null;

    while (all.Count < max)
    {
        List<BlockHeader> batch;
        try
        {
            await SendGetHeaders(session.Peer, locator, ct);
            batch = await WaitForHeaders(session.Peer, ct);
        }
        catch (Exception ex) when (PeerSession.IsPeerDisconnect(ex))
        {
            Console.WriteLine($"    peer dropped, reconnecting…");
            await session.DropAsync();
            if (!await session.EnsureConnectedAsync(all.Count)) break;
            continue;
        }

        if (batch.Count == 0) break;

        foreach (var h in batch)
        {
            if (all.Count >= max) break;
            if (prevHash != null && !h.PrevBlock.AsSpan().SequenceEqual(prevHash))
            {
                Console.WriteLine($"    chain break at height {all.Count + 1}, stopping");
                return all;
            }
            if (!Validator.CheckProofOfWork(h))
            {
                Console.WriteLine($"    PoW failed at height {all.Count + 1}");
                return all;
            }
            store.InsertHeader(h, all.Count + 1);
            all.Add(h);
            prevHash = h.Hash;
        }

        if (batch.Count < 2000) break;
        locator = all[^1].Hash;
    }
    return all;
}
```

Two validation checks per header: chain continuity (each `PrevBlock` must equal the last accepted `Hash`) and proof-of-work. That's enough to reject a peer trying to feed us a fake chain. It's *not* enough to defend against a peer feeding us a chain on a stale fork — for that we'd need to weigh by cumulative work and keep multiple candidates. A toy with one peer at a time gets to assume its peer is honest about which fork it's on.

## The IBD loop — block download

With headers in hand, we walk them in order, asking for each block by hash and validating + persisting the response:

```csharp
for (int i = 0; i < headers.Count; i++)
{
    Block blk;
    try
    {
        await SendGetData(session.Peer, headers[i].Hash, ct);
        blk = await WaitForBlock(session.Peer, ct);
    }
    catch (Exception ex) when (PeerSession.IsPeerDisconnect(ex))
    {
        Log($"    peer dropped at block {i + 1}, reconnecting…");
        await session.DropAsync();
        if (!await session.EnsureConnectedAsync(i)) break;
        i--; // retry this block on the new peer
        continue;
    }

    if (!Validator.CheckProofOfWork(blk.Header) || !Validator.CheckMerkleRoot(blk))
    {
        Log($"    block at height {i + 1} FAILED validation");
        continue;
    }

    int size = 80 + blk.Transactions.Sum(t => t.Size);
    store.InsertBlock(blk, size);
    // …progress reporting omitted
}
```

The `i--; continue` on disconnect is the critical bit — we re-request the block we were waiting on, against the new peer, without losing progress on previously-validated blocks.

## The mempool, briefly

`Mempool.cs` is, on purpose, three lines of meaningful code:

```csharp
public sealed class Mempool
{
    private readonly List<Transaction> _txs = new();
    public IReadOnlyList<Transaction> Transactions => _txs;
    public int Count => _txs.Count;
    public void Add(Transaction tx) => _txs.Add(tx);
    public void Clear() => _txs.Clear();
}
```

We don't populate it during IBD — confirmed transactions inside a block are *not* mempool. The class exists to honour our promise from earlier in the chapter that "the mempool layer can be a list of a tx class." A real mempool listens for `inv tx` / `tx` gossip messages, applies policy filters (min relay fee, RBF, ancestor count), and evicts when full. Adding any of that here would dwarf the rest of the toy. We left it as one screen of code so the layer's *role* — somewhere unconfirmed transactions live — is observable, even if its behaviour isn't.

## Wrapping up

Across these five pages we've built every layer in `chapter-4-architecture`'s table of contents — discovery, communications, persistence, validation, mempool — against real Bitcoin mainnet. It cost ~800 lines of C#. That is, by some way, the smallest interesting Bitcoin client; it elides script execution, the UTXO set, mining, wallets, and consensus rule details. But the *shape* of every layer — what it consumes, what it produces, where the boundaries are — should now be concrete enough to recognise when you read Bitcoin Core, Geth, or Nethermind.

That recognition is the whole point of this chapter. Code that does this much in this little space is rare; it exists because Bitcoin's protocol is an unusually clean separation of concerns, where each layer's job is small enough to fit on one page.
