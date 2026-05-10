## Toy node — Viewing what we stored

A persistence layer that you can only write to is half a persistence layer. We've spent the last few pages downloading and validating blocks into `node.db`, and we promised earlier that the SQLite schema lets us get them back out. This page closes that loop with a small `view` subcommand: `dotnet run -- view <height|hash>` and the toy prints the block.

It also forces an honest admission. When we designed `IBlockStore` we deliberately kept the interface narrow:

> *We deliberately don't add `GetHeader`, `GetTip`, or any read API — the toy doesn't read its own data back during a single run; it just streams forward and persists.*

That was true for IBD. As soon as we want to inspect a block, we need read methods. So we extend the interface — minimally:

```csharp
public interface IBlockStore : IDisposable
{
    void InsertHeader(BlockHeader header, int height);
    void InsertBlock(Block block, int totalSize);
    StoredBlock? GetByHeight(int height);
    StoredBlock? GetByHash(byte[] hashLE);
}

public sealed record StoredBlock(
    int Height,
    BlockHeader Header,
    int TxCount,
    int Size,
    IReadOnlyList<StoredTx> Transactions);

public sealed record StoredTx(byte[] Txid, int Size);
```

Two lookups, two record types. The records exist so the SQLite-specific code stays inside `Storage.cs`; the rest of the program never sees a `SqliteDataReader`.

## The SQLite read

`GetByHeight` is a one-line index lookup that delegates to `GetByHash`. The interesting one is `GetByHash`:

```csharp
public StoredBlock? GetByHash(byte[] hashLE)
{
    int height;
    BlockHeader header;
    using (var hdrCmd = _conn.CreateCommand())
    {
        hdrCmd.CommandText = "SELECT height, raw FROM headers WHERE hash = @h LIMIT 1;";
        hdrCmd.Parameters.AddWithValue("@h", hashLE);
        using var r = hdrCmd.ExecuteReader();
        if (!r.Read()) return null;
        height = r.GetInt32(0);
        header = BlockHeader.Parse((byte[])r.GetValue(1));
    }

    int txCount = 0, size = 0;
    using (var blockCmd = _conn.CreateCommand())
    {
        blockCmd.CommandText = "SELECT tx_count, size FROM blocks WHERE hash = @h LIMIT 1;";
        blockCmd.Parameters.AddWithValue("@h", hashLE);
        using var r = blockCmd.ExecuteReader();
        if (r.Read()) { txCount = r.GetInt32(0); size = r.GetInt32(1); }
    }

    var txs = new List<StoredTx>();
    using (var txCmd = _conn.CreateCommand())
    {
        txCmd.CommandText = "SELECT txid, size FROM transactions WHERE block_hash = @h ORDER BY rowid;";
        txCmd.Parameters.AddWithValue("@h", hashLE);
        using var r = txCmd.ExecuteReader();
        while (r.Read()) txs.Add(new StoredTx((byte[])r.GetValue(0), r.GetInt32(1)));
    }

    return new StoredBlock(height, header, txCount, size, txs);
}
```

Two things worth flagging:

1. **We rebuild `BlockHeader` from the stored `raw` blob, not from the parsed columns.** `BlockHeader.Parse` is the canonical parser. If we ever changed how header fields are stored (or added new ones), the `Hash` we compute from `raw` will still be right, because hash is `dsha256(raw)`.
2. **`ORDER BY rowid` on the transactions read** preserves block insertion order — the coinbase comes back first, exactly where it was in the original block. Without it SQLite is free to return rows in any order, which would break "the coinbase is index 0" semantics.

## The CLI dispatch

`Main` now has a tiny dispatcher: if the first arg is `view`, we go down the view path; otherwise the existing IBD path runs.

```csharp
public static async Task Main(string[] args)
{
    if (args.Length > 0 && args[0] == "view")
    {
        RunViewCommand(args.Skip(1).ToArray());
        return;
    }
    // …existing IBD path
}

private static void RunViewCommand(string[] args)
{
    if (args.Length == 0)
    {
        Console.WriteLine("usage: dotnet run -- view <height|hash>");
        return;
    }

    using IBlockStore store = new SqliteBlockStore("node.db");
    StoredBlock? blk;

    if (int.TryParse(args[0], out var height))
    {
        blk = store.GetByHeight(height);
    }
    else
    {
        var hashLE = TryParseDisplayHash(args[0]);
        if (hashLE == null) { Console.WriteLine("not a valid height or hash"); return; }
        blk = store.GetByHash(hashLE);
    }

    if (blk == null) { Console.WriteLine($"no block found for '{args[0]}'"); return; }
    PrintBlock(blk);
}
```

Note `TryParseDisplayHash`: Bitcoin shows hashes in big-endian, but we store the on-wire little-endian bytes. So a user typing `00000000839a8e…` needs the bytes reversed before lookup.

```csharp
private static byte[]? TryParseDisplayHash(string s)
{
    if (s.Length != 64) return null;
    try
    {
        var be = Convert.FromHexString(s);
        Array.Reverse(be);
        return be;
    }
    catch { return null; }
}
```

This is the same dance Bitcoin Core does internally — the difference between the *displayed* hash and the *stored* hash is purely a presentation convention.

## Pretty printing

```csharp
private static void PrintBlock(StoredBlock b)
{
    var ts = DateTimeOffset.FromUnixTimeSeconds(b.Header.Timestamp).UtcDateTime;
    Console.WriteLine($"Block {b.Height}");
    Console.WriteLine($"  hash       {Crypto.ToHexBigEndian(b.Header.Hash)}");
    Console.WriteLine($"  prev       {Crypto.ToHexBigEndian(b.Header.PrevBlock)}");
    Console.WriteLine($"  merkle     {Crypto.ToHexBigEndian(b.Header.MerkleRoot)}");
    Console.WriteLine($"  version    0x{b.Header.Version:x8}");
    Console.WriteLine($"  timestamp  {b.Header.Timestamp}  ({ts:yyyy-MM-dd HH:mm:ss} UTC)");
    Console.WriteLine($"  bits       0x{b.Header.Bits:x8}");
    Console.WriteLine($"  nonce      {b.Header.Nonce}");
    Console.WriteLine($"  size       {b.Size:N0} B");
    Console.WriteLine();
    Console.WriteLine($"Transactions ({b.TxCount}):");
    for (int i = 0; i < b.Transactions.Count; i++)
    {
        var t = b.Transactions[i];
        var label = i == 0 ? "coinbase" : "";
        Console.WriteLine($"  [{i,4}] {Crypto.ToHexBigEndian(t.Txid)}  {t.Size,5} B  {label}");
    }
}
```

## What you see

After running IBD:

```
$ dotnet run -- view 1
Block 1
  hash       00000000839a8e6886ab5951d76f411475428afc90947ee320161bbf18eb6048
  prev       000000000019d6689c085ae165831e934ff763ae46a2a6c172b3f1b60a8ce26f
  merkle     0e3e2357e806b6cdb1f70b54c3a3a17b6714ee1f0e68bebb44a74b1efd512098
  version    0x00000001
  timestamp  1231469665  (2009-01-09 02:54:25 UTC)
  bits       0x1d00ffff
  nonce      2573394689
  size       214 B

Transactions (1):
  [   0] 0e3e2357e806b6cdb1f70b54c3a3a17b6714ee1f0e68bebb44a74b1efd512098    134 B  coinbase
```

Everything matches what [a block explorer](https://mempool.space/block/00000000839a8e6886ab5951d76f411475428afc90947ee320161bbf18eb6048) shows for height 1: same hash, same `prev` (which is the genesis hash, displayed big-endian), same merkle root, same single coinbase, same timestamp. We built that observation from raw bytes pulled off a TCP socket and validated against proof-of-work.

You can also look up by hash:

```bash
dotnet run -- view 00000000839a8e6886ab5951d76f411475428afc90947ee320161bbf18eb6048
```

## What this view *can't* show

We're bounded by what we stored. The persistence page already noted:

- `transactions` only carries `txid + size + parent_block`, not raw bytes. So we can't print a tx's inputs, outputs, scripts, or amounts.
- We don't store the witness data, so segwit txs lose their witness fields entirely.
- We don't reconstruct block hashes from raw bytes for the *transactions* (only the header), so we trust our parser's txid.

If you wanted a block explorer in this app, the next-step change would be a `tx_raw BLOB` column on the `transactions` table — written during `InsertBlock` from the same span we already parsed. Then `Transaction.Parse` could be reused to re-decode and pretty-print inputs/outputs from the stored bytes. That'd add maybe 30 more lines to the toy.

The bigger lesson is the one this page demonstrates: *the schema is a contract about what your future self can ask*. We made `IBlockStore` narrow on purpose, and now we're paying the (small) cost of widening it. That's the normal lifecycle of a persistence interface — it grows in response to reads, not just writes.
