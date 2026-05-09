using System.Security.Cryptography;

namespace BookBitcoinNode;

public static class Crypto
{
    public static byte[] DoubleSha256(ReadOnlySpan<byte> data)
    {
        Span<byte> first = stackalloc byte[32];
        SHA256.HashData(data, first);
        var result = new byte[32];
        SHA256.HashData(first, result);
        return result;
    }

    // Bitcoin stores hashes little-endian internally but displays them big-endian.
    public static string ToHexBigEndian(byte[] hashLittleEndian)
    {
        var reversed = (byte[])hashLittleEndian.Clone();
        Array.Reverse(reversed);
        return Convert.ToHexString(reversed).ToLowerInvariant();
    }
}
