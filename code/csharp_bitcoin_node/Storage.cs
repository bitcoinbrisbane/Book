using Microsoft.Data.Sqlite;

namespace BookBitcoinNode;

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

public sealed class SqliteBlockStore : IBlockStore
{
    private readonly SqliteConnection _conn;

    public SqliteBlockStore(string path)
    {
        _conn = new SqliteConnection($"Data Source={path}");
        _conn.Open();
        using var cmd = _conn.CreateCommand();
        cmd.CommandText = """
            CREATE TABLE IF NOT EXISTS headers (
                hash      BLOB PRIMARY KEY,
                height    INTEGER NOT NULL,
                prev      BLOB NOT NULL,
                merkle    BLOB NOT NULL,
                timestamp INTEGER NOT NULL,
                bits      INTEGER NOT NULL,
                nonce     INTEGER NOT NULL,
                raw       BLOB NOT NULL
            );
            CREATE INDEX IF NOT EXISTS idx_headers_height ON headers(height);

            CREATE TABLE IF NOT EXISTS blocks (
                hash     BLOB PRIMARY KEY,
                tx_count INTEGER NOT NULL,
                size     INTEGER NOT NULL
            );

            CREATE TABLE IF NOT EXISTS transactions (
                txid       BLOB PRIMARY KEY,
                block_hash BLOB NOT NULL,
                size       INTEGER NOT NULL
            );
            """;
        cmd.ExecuteNonQuery();
    }

    public void InsertHeader(BlockHeader h, int height)
    {
        using var cmd = _conn.CreateCommand();
        cmd.CommandText = """
            INSERT OR IGNORE INTO headers (hash, height, prev, merkle, timestamp, bits, nonce, raw)
            VALUES (@hash, @height, @prev, @merkle, @ts, @bits, @nonce, @raw);
            """;
        cmd.Parameters.AddWithValue("@hash",   h.Hash);
        cmd.Parameters.AddWithValue("@height", height);
        cmd.Parameters.AddWithValue("@prev",   h.PrevBlock);
        cmd.Parameters.AddWithValue("@merkle", h.MerkleRoot);
        cmd.Parameters.AddWithValue("@ts",     h.Timestamp);
        cmd.Parameters.AddWithValue("@bits",   h.Bits);
        cmd.Parameters.AddWithValue("@nonce",  h.Nonce);
        cmd.Parameters.AddWithValue("@raw",    h.Raw);
        cmd.ExecuteNonQuery();
    }

    public void InsertBlock(Block block, int totalSize)
    {
        using var tx = _conn.BeginTransaction();
        using (var cmd = _conn.CreateCommand())
        {
            cmd.Transaction = tx;
            cmd.CommandText = """
                INSERT OR IGNORE INTO blocks (hash, tx_count, size)
                VALUES (@hash, @count, @size);
                """;
            cmd.Parameters.AddWithValue("@hash",  block.Header.Hash);
            cmd.Parameters.AddWithValue("@count", block.Transactions.Count);
            cmd.Parameters.AddWithValue("@size",  totalSize);
            cmd.ExecuteNonQuery();
        }
        foreach (var t in block.Transactions)
        {
            using var cmd = _conn.CreateCommand();
            cmd.Transaction = tx;
            cmd.CommandText = """
                INSERT OR IGNORE INTO transactions (txid, block_hash, size)
                VALUES (@txid, @bhash, @size);
                """;
            cmd.Parameters.AddWithValue("@txid",  t.Txid);
            cmd.Parameters.AddWithValue("@bhash", block.Header.Hash);
            cmd.Parameters.AddWithValue("@size",  t.Size);
            cmd.ExecuteNonQuery();
        }
        tx.Commit();
    }

    public StoredBlock? GetByHeight(int height)
    {
        using var cmd = _conn.CreateCommand();
        cmd.CommandText = "SELECT hash FROM headers WHERE height = @h LIMIT 1;";
        cmd.Parameters.AddWithValue("@h", height);
        return cmd.ExecuteScalar() is byte[] hash ? GetByHash(hash) : null;
    }

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

    public void Dispose() => _conn.Dispose();
}
