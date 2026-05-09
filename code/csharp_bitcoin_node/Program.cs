using System.Buffers.Binary;
using System.Diagnostics;
using System.Net;

namespace BookBitcoinNode;

public class Program
{
    // Genesis block hash, in on-the-wire little-endian byte order.
    // Big-endian display: 000000000019d6689c085ae165831e934ff763ae46a2a6c172b3f1b60a8ce26f
    private static readonly byte[] GenesisHashLE = Convert.FromHexString(
        "6fe28c0ab6f1b372c1a6a246ae63f74f931e8365e15a089c68d6190100000000");

    public static async Task Main(string[] args)
    {
        int maxBlocks = args.Length > 0 && int.TryParse(args[0], out var n) ? n : 1000;
        var ct = CancellationToken.None;

        Console.WriteLine("[1] Discovering peers via DNS seeds…");
        var endpoints = await Discovery.ResolveSeedsAsync(ct);
        Console.WriteLine($"    {endpoints.Count} candidate peers\n");

        Console.WriteLine("[2] Connecting to a peer…");
        await using var peer = new PeerConnection();
        var connected = await TryConnect(peer, endpoints, ct);
        if (connected == null) { Console.WriteLine("    no peers reachable"); return; }
        Console.WriteLine($"    connected to {connected}\n");

        Console.WriteLine("[3] Handshaking…");
        await peer.HandshakeAsync(0, ct);
        Console.WriteLine("    handshake complete\n");

        using var store = new SqliteBlockStore("node.db");

        Console.WriteLine($"[4] Header sync (target: {maxBlocks} blocks)…");
        var headers = await DownloadHeaders(peer, store, maxBlocks, ct);
        Console.WriteLine($"    {headers.Count} headers validated and stored\n");

        Console.WriteLine("[5] Block download…");
        var sw = Stopwatch.StartNew();
        int valid = 0, totalTxs = 0;
        long totalBytes = 0;

        for (int i = 0; i < headers.Count; i++)
        {
            await SendGetData(peer, headers[i].Hash, ct);
            var blk = await WaitForBlock(peer, ct);

            if (!Validator.CheckProofOfWork(blk.Header) || !Validator.CheckMerkleRoot(blk))
            {
                Console.WriteLine($"    block at height {i + 1} FAILED validation");
                continue;
            }

            int size = 80 + blk.Transactions.Sum(t => t.Size);
            store.InsertBlock(blk, size);
            valid++;
            totalTxs += blk.Transactions.Count;
            totalBytes += size;

            if ((i + 1) % 500 == 0 || i + 1 == headers.Count)
            {
                var rate = (i + 1) / sw.Elapsed.TotalSeconds;
                Console.WriteLine(
                    $"    {i + 1}/{headers.Count}   {rate:F0} blocks/s   {totalTxs} txs   {totalBytes / 1024:N0} KiB");
            }
        }

        Console.WriteLine($"\n    {valid}/{headers.Count} blocks valid in {sw.Elapsed.TotalSeconds:F1}s");
        Console.WriteLine($"    {totalTxs} transactions, {totalBytes / 1024:N0} KiB persisted to node.db");
        Console.WriteLine("\nDone.");
    }

    // Walks the chain forward, one `getheaders` round per 2000 headers, until either
    // the peer has nothing more or we hit `max`. Each header is checked for chain
    // continuity (prev == last) and proof-of-work before persistence.
    private static async Task<List<BlockHeader>> DownloadHeaders(
        PeerConnection peer, SqliteBlockStore store, int max, CancellationToken ct)
    {
        var all = new List<BlockHeader>();
        var locator = GenesisHashLE;
        byte[]? prevHash = null;

        while (all.Count < max)
        {
            await SendGetHeaders(peer, locator, ct);
            var batch = await WaitForHeaders(peer, ct);
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
                    Console.WriteLine($"    PoW failed at height {all.Count + 1}, stopping");
                    return all;
                }
                store.InsertHeader(h, all.Count + 1);
                all.Add(h);
                prevHash = h.Hash;
            }

            Console.WriteLine($"    headers: {all.Count}");
            if (batch.Count < 2000) break;
            locator = all[^1].Hash;
        }

        return all;
    }

    private static async Task<IPEndPoint?> TryConnect(
        PeerConnection peer, List<IPEndPoint> endpoints, CancellationToken ct)
    {
        foreach (var ep in endpoints.Take(10))
        {
            try
            {
                using var timeout = CancellationTokenSource.CreateLinkedTokenSource(ct);
                timeout.CancelAfter(TimeSpan.FromSeconds(5));
                await peer.ConnectAsync(ep, timeout.Token);
                return ep;
            }
            catch (Exception ex)
            {
                Console.WriteLine($"    {ep}: {ex.Message}");
            }
        }
        return null;
    }

    private static async Task SendGetHeaders(
        PeerConnection peer, byte[] locatorHashLE, CancellationToken ct)
    {
        await peer.SendAsync("getheaders", BuildGetHeadersPayload(locatorHashLE), ct);
    }

    private static async Task SendGetData(
        PeerConnection peer, byte[] blockHashLE, CancellationToken ct)
    {
        await peer.SendAsync("getdata", BuildGetDataPayload(blockHashLE), ct);
    }

    private static byte[] BuildGetHeadersPayload(byte[] locatorHashLE)
    {
        using var ms = new MemoryStream();
        var tmp = new byte[4];
        BinaryPrimitives.WriteInt32LittleEndian(tmp, NetworkConstants.ProtocolVersion);
        ms.Write(tmp);
        Varint.Write(ms, 1);
        ms.Write(locatorHashLE);
        ms.Write(new byte[32]); // hash_stop = zeros means "send as many as you can"
        return ms.ToArray();
    }

    private static byte[] BuildGetDataPayload(byte[] blockHashLE)
    {
        using var ms = new MemoryStream();
        Varint.Write(ms, 1);
        var typ = new byte[4];
        BinaryPrimitives.WriteUInt32LittleEndian(typ, 2); // MSG_BLOCK
        ms.Write(typ);
        ms.Write(blockHashLE);
        return ms.ToArray();
    }

    private static async Task<List<BlockHeader>> WaitForHeaders(
        PeerConnection peer, CancellationToken ct)
    {
        while (true)
        {
            var msg = await peer.ReceiveAsync(ct);
            if (msg.Command == "ping") { await peer.SendAsync("pong", msg.Payload, ct); continue; }
            if (msg.Command == "headers") return ParseHeaders(msg.Payload);
        }
    }

    private static async Task<Block> WaitForBlock(PeerConnection peer, CancellationToken ct)
    {
        while (true)
        {
            var msg = await peer.ReceiveAsync(ct);
            if (msg.Command == "ping") { await peer.SendAsync("pong", msg.Payload, ct); continue; }
            if (msg.Command == "block") return Block.Parse(msg.Payload);
        }
    }

    private static List<BlockHeader> ParseHeaders(byte[] payload)
    {
        int p = 0;
        p += Varint.Read(payload.AsSpan(p), out var count);
        var result = new List<BlockHeader>((int)count);
        for (ulong i = 0; i < count; i++)
        {
            result.Add(BlockHeader.Parse(payload.AsSpan(p, 80)));
            p += 80;
            p += Varint.Read(payload.AsSpan(p), out _); // tx_count, always 0 in headers msg
        }
        return result;
    }
}
