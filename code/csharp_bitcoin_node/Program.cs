using System.Buffers.Binary;
using System.Diagnostics;

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

        await using var session = new PeerSession(endpoints, ct);

        Console.WriteLine("[2/3] Connecting + handshaking…");
        if (!await session.EnsureConnectedAsync(0))
        {
            Console.WriteLine("    no peers reachable");
            return;
        }
        Console.WriteLine();

        using IBlockStore store = new SqliteBlockStore("node.db");

        Console.WriteLine($"[4] Header sync (target: {maxBlocks} blocks)…");
        var headers = await DownloadHeaders(session, store, maxBlocks, ct);
        Console.WriteLine($"    {headers.Count} headers validated and stored\n");

        if (headers.Count == 0) return;

        Console.WriteLine("[5] Block download…");
        await DownloadBlocks(session, headers, store, ct);
        Console.WriteLine("\nDone.");
    }

    // Walks the chain forward, one `getheaders` round per 2000 headers, until either
    // the peer has nothing more or we hit `max`. Each header is checked for chain
    // continuity (prev == last) and proof-of-work before persistence. If the peer
    // drops, we fall back to the next candidate and resume from the latest hash.
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
                Console.WriteLine($"    peer dropped during header sync ({ex.GetType().Name}), reconnecting…");
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

    private static async Task DownloadBlocks(
        PeerSession session, List<BlockHeader> headers, IBlockStore store, CancellationToken ct)
    {
        var sw = Stopwatch.StartNew();
        int valid = 0, totalTxs = 0;
        int maxTxsInBlock = 0;
        long totalBytes = 0;

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
                Log($"    peer dropped at block {i + 1} ({ex.GetType().Name}), reconnecting…");
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
            valid++;
            totalTxs += blk.Transactions.Count;
            if (blk.Transactions.Count > maxTxsInBlock) maxTxsInBlock = blk.Transactions.Count;
            totalBytes += size;

            int height = i + 1;
            double elapsed = Math.Max(0.001, sw.Elapsed.TotalSeconds);
            double rate = height / elapsed;
            int remaining = headers.Count - height;
            TimeSpan eta = rate > 0 ? TimeSpan.FromSeconds(remaining / rate) : TimeSpan.Zero;
            string shortHash = Crypto.ToHexBigEndian(blk.Header.Hash)[..16];

            Status(
                $"    [{height,6}/{headers.Count}] {shortHash}…  " +
                $"{blk.Transactions.Count,4} tx  {FormatBytes(size),9}  " +
                $"|  total {totalTxs,7:N0} txs / {FormatBytes(totalBytes),9}  " +
                $"|  {rate,5:F1} blk/s  ETA {FormatEta(eta)}");

            // Highlight any block above 100 txs as "fat" — early in the chain those are interesting.
            if (blk.Transactions.Count >= 100)
            {
                Log($"    fat block #{height}: {blk.Transactions.Count:N0} txs, {FormatBytes(size)}, hash {shortHash}…");
            }

            // Permanent checkpoint every 100 blocks so scrollback shows the curve.
            if (height % 100 == 0 || height == headers.Count)
            {
                Log($"    ── {height,6}/{headers.Count}  total {totalTxs,7:N0} txs / {FormatBytes(totalBytes),9}  " +
                    $"avg {rate:F1} blk/s  ETA {FormatEta(eta)} ──");
            }
        }

        Log("");
        Log($"    {valid}/{headers.Count} blocks valid in {sw.Elapsed.TotalSeconds:F1}s ({valid / Math.Max(0.001, sw.Elapsed.TotalSeconds):F1} blk/s avg)");
        Log($"    {totalTxs:N0} transactions ({maxTxsInBlock:N0} tx max in a single block)");
        Log($"    {FormatBytes(totalBytes)} persisted to node.db");
    }

    // --- Console helpers: a single mutable status line with checkpoint logging ---

    private static int _lastStatusLen;

    private static void Status(string line)
    {
        var padding = Math.Max(0, _lastStatusLen - line.Length);
        Console.Write("\r" + line + new string(' ', padding));
        _lastStatusLen = line.Length;
    }

    private static void Log(string line)
    {
        if (_lastStatusLen > 0)
        {
            Console.Write("\r" + new string(' ', _lastStatusLen) + "\r");
            _lastStatusLen = 0;
        }
        Console.WriteLine(line);
    }

    private static string FormatBytes(long bytes) =>
        bytes >= 1024 * 1024 ? $"{bytes / 1024.0 / 1024:F1} MiB" :
        bytes >= 1024 ? $"{bytes / 1024.0:F1} KiB" :
        $"{bytes} B";

    private static string FormatEta(TimeSpan t) =>
        t.TotalHours >= 1 ? $"{(int)t.TotalHours}h{t.Minutes:D2}m" :
        t.TotalMinutes >= 1 ? $"{(int)t.TotalMinutes}m{t.Seconds:D2}s" :
        $"{(int)Math.Ceiling(t.TotalSeconds)}s";

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
