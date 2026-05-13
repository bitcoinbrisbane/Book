## Toy node — Persistence

Once a block is validated, somewhere has to store it. We promised earlier in this chapter to compare four databases and that "all databases use files to store data." For the toy node we picked SQLite — embedded, no daemon, no setup, file-on-disk. It's not what Bitcoin Core uses (LevelDB for the chainstate, flat files for blocks), but it's perfect for a teaching example.

The whole layer is in `Storage.cs`.

## The interface

We start with an interface, not a concrete class. This is a small refactor that pays off immediately: tests can drop in an in-memory store, and a future page in this chapter could replace SQLite with RocksDB without changing a line of orchestration code.

```csharp
public interface IBlockStore : IDisposable
{
    void InsertHeader(BlockHeader header, int height);
    void InsertBlock(Block block, int totalSize);
}
```

That's it. Two write methods, dispose. We deliberately don't add `GetHeader`, `GetTip`, or any read API — the toy doesn't read its own data back during a single run; it just streams forward and persists. Dependency boundaries should be as small as the consumer needs.

`Program.cs` then types the variable to the interface, not the implementation:

```csharp
using IBlockStore store = new SqliteBlockStore("node.db");
```

## The SQLite implementation

Three tables, no foreign keys, indexed by hash for lookup. The schema is created idempotently on construction:

```csharp
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
```

A few choices worth pointing at:

- **Hash columns are `BLOB`, not `TEXT`.** Bitcoin hashes are 32 bytes of binary; storing them as hex would double the space and slow comparison. SQLite handles BLOB primary keys fine.
- **`headers` carries the raw 80 bytes.** With it we can recompute the hash any time. Without it we'd be trusting our own (potentially buggy) parser to be canonical.
- **`transactions` only stores the txid + parent block + size.** A real node also stores the raw bytes and an output index, but for "did we see this tx" purposes the txid alone is enough.

## Inserts

```csharp
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
```

`INSERT OR IGNORE` makes the call idempotent — re-running with the same chain doesn't error. That matters because IBD over a flaky peer connection can replay the same block on retry.

`InsertBlock` wraps the block + each transaction insert in a single SQLite transaction:

```csharp
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
```

The transaction guarantees we never end up with a "block exists but its transactions don't" state if the process is killed mid-write. For a 2000-tx block on a fast disk this is faster than no transaction (one fsync vs 2001).

## What this layer skips

- **No UTXO set.** The single biggest piece of state a real node maintains. UTXO updates are *the* hot loop; LevelDB is what makes Bitcoin Core fast at it.
- **No block file storage.** Real nodes write raw block bytes to flat append-only files (`blk00000.dat`, `blk00001.dat`, …) and use the database only for an index. Putting raw block bytes in SQL would scale poorly past a few GB.
- **No reorg support.** A reorg requires being able to "undo" inserts. We'd need an undo log per block to do that; we don't have one.
- **No write-batching across blocks.** We start a SQLite transaction per block. For full-history IBD you'd batch dozens of blocks per transaction.

The interface boundary makes any of these a contained future change. That's the point of having an interface.
