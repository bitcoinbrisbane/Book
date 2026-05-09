using System.Buffers.Binary;
using System.Net;
using System.Net.Sockets;
using System.Text;

namespace BookBitcoinNode;

public static class NetworkConstants
{
    public const uint MainnetMagic = 0xD9B4BEF9;
    public const int MainnetPort = 8333;
    public const int ProtocolVersion = 70016;
    public const ulong ServicesNodeNetwork = 1;
    public const string UserAgent = "/BookExampleNode:0.1/";

    public static readonly string[] DnsSeeds =
    {
        "seed.bitcoin.sipa.be",
        "dnsseed.bluematt.me",
        "seed.bitcoinstats.com",
        "seed.bitcoin.jonasschnelli.ch",
        "seed.btc.petertodd.org",
        "seed.bitcoin.sprovoost.nl",
        "dnsseed.emzy.de",
        "seed.bitcoin.wiz.biz",
    };
}

public static class Varint
{
    public static int Read(ReadOnlySpan<byte> buf, out ulong value)
    {
        byte first = buf[0];
        if (first < 0xfd) { value = first; return 1; }
        if (first == 0xfd) { value = BinaryPrimitives.ReadUInt16LittleEndian(buf[1..]); return 3; }
        if (first == 0xfe) { value = BinaryPrimitives.ReadUInt32LittleEndian(buf[1..]); return 5; }
        value = BinaryPrimitives.ReadUInt64LittleEndian(buf[1..]); return 9;
    }

    public static void Write(Stream s, ulong value)
    {
        if (value < 0xfd) { s.WriteByte((byte)value); return; }
        Span<byte> b = stackalloc byte[8];
        if (value <= 0xffff)
        {
            s.WriteByte(0xfd);
            BinaryPrimitives.WriteUInt16LittleEndian(b, (ushort)value);
            s.Write(b[..2]);
        }
        else if (value <= 0xffffffff)
        {
            s.WriteByte(0xfe);
            BinaryPrimitives.WriteUInt32LittleEndian(b, (uint)value);
            s.Write(b[..4]);
        }
        else
        {
            s.WriteByte(0xff);
            BinaryPrimitives.WriteUInt64LittleEndian(b, value);
            s.Write(b);
        }
    }
}

public sealed record P2PMessage(string Command, byte[] Payload);

public sealed class PeerConnection : IAsyncDisposable
{
    private readonly TcpClient _tcp = new();
    private NetworkStream? _stream;

    public async Task ConnectAsync(IPEndPoint endpoint, CancellationToken ct)
    {
        await _tcp.ConnectAsync(endpoint.Address, endpoint.Port, ct);
        _stream = _tcp.GetStream();
    }

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

    private async Task ReadExactAsync(Memory<byte> buf, CancellationToken ct)
    {
        var read = 0;
        while (read < buf.Length)
        {
            var n = await _stream!.ReadAsync(buf[read..], ct);
            if (n == 0) throw new EndOfStreamException();
            read += n;
        }
    }

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
        ms.WriteByte(0);

        return ms.ToArray();
    }

    private static void WriteNetAddr(Stream s, IPAddress ip, ushort port)
    {
        Span<byte> buf = stackalloc byte[26];
        BinaryPrimitives.WriteUInt64LittleEndian(buf[..8], NetworkConstants.ServicesNodeNetwork);
        var ipv6 = ip.MapToIPv6().GetAddressBytes();
        ipv6.CopyTo(buf.Slice(8, 16));
        BinaryPrimitives.WriteUInt16BigEndian(buf.Slice(24, 2), port);
        s.Write(buf);
    }

    public async ValueTask DisposeAsync()
    {
        if (_stream != null) await _stream.DisposeAsync();
        _tcp.Dispose();
    }
}
