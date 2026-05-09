using System.Buffers.Binary;
using System.Numerics;

namespace BookBitcoinNode;

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

    public static BlockHeader Parse(ReadOnlySpan<byte> bytes)
    {
        return new BlockHeader
        {
            Version = BinaryPrimitives.ReadInt32LittleEndian(bytes[..4]),
            PrevBlock = bytes.Slice(4, 32).ToArray(),
            MerkleRoot = bytes.Slice(36, 32).ToArray(),
            Timestamp = BinaryPrimitives.ReadUInt32LittleEndian(bytes.Slice(68, 4)),
            Bits = BinaryPrimitives.ReadUInt32LittleEndian(bytes.Slice(72, 4)),
            Nonce = BinaryPrimitives.ReadUInt32LittleEndian(bytes.Slice(76, 4)),
            Raw = bytes[..80].ToArray(),
        };
    }

    // Decompress nBits into the 256-bit target threshold a valid hash must fall under.
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
}

public sealed class Transaction
{
    public byte[] Txid { get; init; } = new byte[32];
    public int Size { get; init; }

    // Parses one tx from a block stream and returns the txid (the legacy hash, no witness).
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

        return new Transaction
        {
            Txid = Crypto.DoubleSha256(forHash),
            Size = p,
        };
    }
}

public sealed class Block
{
    public BlockHeader Header { get; init; } = null!;
    public List<Transaction> Transactions { get; init; } = new();

    public static Block Parse(ReadOnlySpan<byte> bytes)
    {
        var header = BlockHeader.Parse(bytes[..80]);
        int p = 80;
        p += Varint.Read(bytes[p..], out var txCount);
        var txs = new List<Transaction>((int)txCount);
        for (ulong i = 0; i < txCount; i++)
        {
            var tx = Transaction.Parse(bytes[p..], out int consumed);
            txs.Add(tx);
            p += consumed;
        }
        return new Block { Header = header, Transactions = txs };
    }

    public byte[] ComputeMerkleRoot()
    {
        var hashes = Transactions.Select(t => t.Txid).ToList();
        if (hashes.Count == 0) return new byte[32];
        while (hashes.Count > 1)
        {
            if (hashes.Count % 2 == 1) hashes.Add(hashes[^1]);
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
}

public static class Validator
{
    public static bool CheckProofOfWork(BlockHeader header)
    {
        var hashAsNumber = new BigInteger(header.Hash, isUnsigned: true, isBigEndian: false);
        return hashAsNumber <= header.Target;
    }

    public static bool CheckMerkleRoot(Block block)
    {
        var computed = block.ComputeMerkleRoot();
        return computed.AsSpan().SequenceEqual(block.Header.MerkleRoot);
    }
}
