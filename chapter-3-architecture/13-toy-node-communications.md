## Toy node — Communications

Once we've got an IP and a port, we need to actually talk Bitcoin's wire protocol. That's three things: **framing** (how messages are delimited on the wire), **encoding** (varint, network addresses, the version payload), and the **handshake** (which messages must be exchanged before anything else is allowed).

Everything in this section lives in `Network.cs` and `PeerSession.cs`.

## Constants

```csharp
public static class NetworkConstants
{
    public const uint MainnetMagic = 0xD9B4BEF9;
    public const int MainnetPort = 8333;
    public const int ProtocolVersion = 70016;
    public const ulong ServicesNodeNetwork = 1;
    public const string UserAgent = "/BookExampleNode:0.1/";
    // …DnsSeeds list omitted — see previous page
}
```

The magic number 0xD9B4BEF9 prefixes every Bitcoin mainnet message. Testnet, regtest, and signet all use different magic. It's the first thing the receiver checks; a wrong magic means "this isn't even a Bitcoin message" and we hang up.

## Message framing

Every Bitcoin P2P message has a 24-byte header followed by a payload:

```
+--------+----------------+--------+-----------+
| magic  | command (12B)  | length | checksum  |  +  payload (length bytes)
| 4B LE  | ASCII null-pad | 4B LE  | 4B (DSHA) |
+--------+----------------+--------+-----------+
```

Sending one is straightforward:

```csharp
public async Task SendAsync(string command, byte[] payload, CancellationToken ct)
{
    var header = new byte[24];
    BinaryPrimitives.WriteUInt32LittleEndian(header.AsSpan(0, 4), NetworkConstants.MainnetMagic);
    var cmdBytes = Encoding.ASCII.GetBytes(command);
    Array.Copy(cmdBytes, 0, header, 4, Math.Min(cmdBytes.Length, 12));
    BinaryPrimitives.WriteUInt32LittleEndian(header.AsSpan(16, 4), (uint)payload.Length);
    var checksum = Crypto.DoubleSha256(payload);
    Array.Copy(checksum, 0, header, 20, 4);

    await _stream!.WriteAsync(header, ct);
    if (payload.Length > 0) await _stream.WriteAsync(payload, ct);
}
```

Receiving inverts that:

```csharp
public async Task<P2PMessage> ReceiveAsync(CancellationToken ct)
{
    var header = new byte[24];
    await ReadExactAsync(header, ct);

    uint magic = BinaryPrimitives.ReadUInt32LittleEndian(header.AsSpan(0, 4));
    if (magic != NetworkConstants.MainnetMagic)
        throw new InvalidDataException($"bad magic 0x{magic:x8}");

    var command = Encoding.ASCII.GetString(header, 4, 12).TrimEnd('\0');
    uint length = BinaryPrimitives.ReadUInt32LittleEndian(header.AsSpan(16, 4));
    if (length > 32 * 1024 * 1024) throw new InvalidDataException("oversized message");

    var payload = new byte[length];
    if (length > 0) await ReadExactAsync(payload, ct);

    var checksum = Crypto.DoubleSha256(payload);
    if (!checksum.AsSpan(0, 4).SequenceEqual(header.AsSpan(20, 4)))
        throw new InvalidDataException("checksum mismatch");

    return new P2PMessage(command, payload);
}
```

Three things are worth pointing out:

1. **The checksum is the first 4 bytes of `dsha256(payload)`** — same hash construction used everywhere else in Bitcoin. If your network corrupts a byte you'll detect it immediately. (The checksum doesn't *secure* anything — peers are trusted to not lie about their own checksum — it's just integrity.)
2. **`ReadExactAsync` loops on partial reads.** TCP doesn't guarantee that one `WriteAsync` arrives in one `ReadAsync`. Real network code always loops.
3. **The 32 MiB cap** is a paranoid sanity check. The real protocol allows up to ~32 MiB messages, so this matches; we abort cleanly if a misbehaving peer claims a 4 GB payload.

## Varints

Bitcoin's variable-length integer is everywhere — counts, list lengths, script sizes. One leading byte tells you how many follow:

```csharp
public static int Read(ReadOnlySpan<byte> buf, out ulong value)
{
    byte first = buf[0];
    if (first < 0xfd) { value = first; return 1; }
    if (first == 0xfd) { value = BinaryPrimitives.ReadUInt16LittleEndian(buf[1..]); return 3; }
    if (first == 0xfe) { value = BinaryPrimitives.ReadUInt32LittleEndian(buf[1..]); return 5; }
    value = BinaryPrimitives.ReadUInt64LittleEndian(buf[1..]); return 9;
}
```

Small numbers (most of them) cost 1 byte. Counts that fit in a `ushort` cost 3 bytes. And so on. A typical block has ~2000 transactions — that's a 3-byte varint. The same encoding shows up in every layer below this one.

## The handshake

Bitcoin's "open a connection" ritual is four messages: each side sends `version`, then each side replies `verack`. Until both have arrived, no other message is honoured.

```csharp
public async Task HandshakeAsync(int startHeight, CancellationToken ct)
{
    await SendAsync("version", BuildVersionPayload(startHeight), ct);

    bool gotVersion = false, gotVerack = false;
    while (!gotVersion || !gotVerack)
    {
        var msg = await ReceiveAsync(ct);
        if (msg.Command == "version")
        {
            gotVersion = true;
            await SendAsync("verack", Array.Empty<byte>(), ct);
        }
        else if (msg.Command == "verack")
        {
            gotVerack = true;
        }
    }
}
```

Modern Bitcoin Core peers also send a flurry of optional messages around `verack` — `sendaddrv2`, `wtxidrelay`, `sendcmpct` — to negotiate which improvements both sides support. Our toy ignores them; the peer doesn't care that we don't reply, it just falls back to the default behaviour. That's a deliberate design property of the protocol: every extension is opt-in.

## The version payload

```csharp
private static byte[] BuildVersionPayload(int startHeight)
{
    using var ms = new MemoryStream();
    Span<byte> tmp = stackalloc byte[8];

    BinaryPrimitives.WriteInt32LittleEndian(tmp, NetworkConstants.ProtocolVersion);
    ms.Write(tmp[..4]);
    BinaryPrimitives.WriteUInt64LittleEndian(tmp, NetworkConstants.ServicesNodeNetwork);
    ms.Write(tmp);
    BinaryPrimitives.WriteInt64LittleEndian(tmp, DateTimeOffset.UtcNow.ToUnixTimeSeconds());
    ms.Write(tmp);

    WriteNetAddr(ms, IPAddress.Loopback, 0);
    WriteNetAddr(ms, IPAddress.Loopback, 0);

    BinaryPrimitives.WriteUInt64LittleEndian(tmp, (ulong)Random.Shared.NextInt64());
    ms.Write(tmp);

    var ua = Encoding.ASCII.GetBytes(NetworkConstants.UserAgent);
    Varint.Write(ms, (ulong)ua.Length);
    ms.Write(ua);

    BinaryPrimitives.WriteInt32LittleEndian(tmp, startHeight);
    ms.Write(tmp[..4]);
    ms.WriteByte(0); // relay flag

    return ms.ToArray();
}
```

The fields, in order: protocol version, our service flags, current Unix time, the receiver's address, our own address, a random nonce (used to detect self-connections), our user agent string, the height of our best chain, and a relay flag. The two address fields each take 26 bytes — service flags + IPv6-mapped IP + 2-byte port. We populate ours with `Loopback:0` because it doesn't matter; what matters is that the field is well-formed.

## Falling over to the next peer

A non-trivial fraction of mainnet peers will drop our connection mid-IBD — they're pruned, overloaded, or simply prefer better-behaved clients. `PeerSession` wraps that:

```csharp
public sealed class PeerSession : IAsyncDisposable
{
    private readonly Queue<IPEndPoint> _candidates;
    private PeerConnection? _peer;

    public PeerConnection Peer =>
        _peer ?? throw new InvalidOperationException("not connected");

    public async Task<bool> EnsureConnectedAsync(int startHeight)
    {
        if (_peer != null) return true;
        while (_candidates.Count > 0)
        {
            var ep = _candidates.Dequeue();
            try
            {
                var p = new PeerConnection();
                using var timeout = CancellationTokenSource.CreateLinkedTokenSource(_ct);
                timeout.CancelAfter(TimeSpan.FromSeconds(10));
                await p.ConnectAsync(ep, timeout.Token);
                await p.HandshakeAsync(startHeight, timeout.Token);
                _peer = p;
                Console.WriteLine($"    connected: {ep}");
                return true;
            }
            catch (Exception ex)
            {
                Console.WriteLine($"    {ep}: {ex.Message}");
            }
        }
        return false;
    }

    public static bool IsPeerDisconnect(Exception ex) =>
        ex is EndOfStreamException
        or IOException
        or SocketException
        or InvalidDataException
        or OperationCanceledException;
}
```

The IBD loop catches `IsPeerDisconnect` exceptions, calls `DropAsync()`, then `EnsureConnectedAsync(...)` to roll over to the next DNS-seed candidate, and resumes from the latest persisted block hash. A real node connects to 8–125 peers in parallel and rebalances continuously; we get away with one at a time only because we're a toy.

## What we skipped

- **BIP155 `addrv2`** — multi-network address gossip (Tor v3, I2P).
- **BIP324 v2 transport** — encrypted, authenticated peer connections; we run plaintext.
- **`feefilter`, `sendcmpct`, `wtxidrelay`** — every modern message-type negotiation. We default to the "nothing optional" path.
- **Ban scoring.** A real peer that sends garbage gets `Misbehaving`-incremented and eventually disconnected and ban-listed. We just throw.
