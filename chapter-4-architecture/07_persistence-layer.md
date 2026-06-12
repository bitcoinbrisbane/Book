## Persistence layer

Our blockchain has to save a lot of data, and we have to read it back forever. Every committed block becomes permanent state that future nodes have to validate against. So before we write a line of code, we need to decide *how* that state is stored — and the choice is harder than it looks. Each engine has trade-offs that only show up under load.

This page picks four engines worth knowing, runs a real benchmark, and reads the results.

## Four families

A blockchain's persistence layer typically picks from one of these families. Each shows up somewhere in production:

- **SQLite** — embedded, single file, full SQL, B-tree indexes. Default in small projects because there's nothing to install. Fast with a single transaction wrapping many inserts.
- **PebbleDB** / **RocksDB** — embedded LSM-tree. The shape Bitcoin Core uses for its chainstate (LevelDB, RocksDB's predecessor); the shape Geth, CockroachDB, and TiKV use for almost all storage. Optimised for write-heavy append-mostly workloads — exactly what a chain is.
- **PostgreSQL** — server, B-tree, full ACID. Used by indexers (TheGraph), some Ethereum clients, and Waku for its store node. Strong when you want operational tooling — backups, replicas, schema migrations.
- **MongoDB** — server, document store, schema-flexible. Common when chain data has to be served to a web frontend with arbitrary shapes. Block explorers and analytics tools often put a Mongo collection in front of the chain.

You will hear **Redis** mentioned alongside these. Don't be fooled. Redis is in-memory by default; durability is opt-in via AOF or RDB snapshots. As an *application cache* in front of a blockchain client it's excellent. As the canonical store of chain state it's the wrong shape — losing the mempool isn't catastrophic, losing the UTXO set is.

## The benchmark

To turn this into something measured rather than asserted we wrote [`code/block_db_benchmark`](../code/block_db_benchmark/). It's a single Go program with four `Backend` implementations, all pure-Go (no CGo), so the whole thing compiles in one go. Each run:

1. Inserts 100,000 synthetic block-header records (same schema as our C# toy node — hash, height, prev, merkle, timestamp, bits, nonce, version, tx_count, size, raw 80 bytes).
2. Does 10,000 random point reads by primary-key hash.
3. Does 10,000 random point reads by secondary-index height.
4. Reports time, throughput, and on-disk size.

The random reads use a stable RNG seed so every backend sees the same access pattern.

### Stack

| Component   | Library                                |
| ----------- | -------------------------------------- |
| SQLite      | `modernc.org/sqlite` (pure-Go)         |
| PebbleDB    | `github.com/cockroachdb/pebble`        |
| PostgreSQL  | `github.com/jackc/pgx/v5`              |
| MongoDB     | `go.mongodb.org/mongo-driver`          |

### Strategies

Each backend uses its idiomatic path. We deliberately avoid the absolute-fastest variants (`COPY FROM` for Postgres, `pebble.NoSync` for Pebble) — the goal is to measure what an *ordinary application* would write.

| Backend    | Bulk insert                                            | Hash lookup           | Height lookup                                     |
| ---------- | ------------------------------------------------------ | --------------------- | ------------------------------------------------- |
| SQLite     | one tx, prepared `INSERT` executed N times             | PK `SELECT`           | indexed `SELECT` on `height`                      |
| Postgres   | one tx, prepared `INSERT` executed N times             | PK `SELECT`           | indexed `SELECT` on `height`                      |
| Mongo      | `InsertMany` in batches of 1000, unordered             | `_id` `FindOne`       | indexed `FindOne` on `height`                     |
| Pebble     | one `WriteBatch`, all puts, committed synchronously    | `Get b/<hash>`        | `Get h/<height>` → hash → `Get b/<hash>` (2 hops) |

Pebble's "two hops" is the visible cost of secondary access in a KV store. There is no built-in index on a non-primary field, so we maintain a parallel keyspace: `h/<big-endian-height>` → 32-byte hash. Every height lookup is two reads.

### Running it

```bash
cd code/block_db_benchmark
docker compose up -d                       # Postgres on 5433, Mongo on 27017
go mod tidy                                # first run only
go run . -count 100000 -reads 10000 -backend all
```

## Results

A representative run on a developer laptop (8-core, NVMe):

```
=== Summary ===
    100000 blocks per run, 10000 random reads per access pattern

    backend     insert     insert/s      byHash/s      byHeight/s    disk
    -------     ------     --------      --------      ----------    ----
    sqlite      1.657s     60366         35491         32209         26.21 MiB
    pebble      102ms      984152        977810        535098        29.20 MiB
    postgres    5.892s     16973         16305         16223         42.19 MiB
    mongo       1.003s     99659         8460          7351          13.61 MiB
```

Numbers will move with CPU, fsync behaviour, network round-trip, and disk write cache. The *shape* is what matters, and it stays consistent across reruns. Four readings:

**1. The fastest engine for inserts is also the fastest for reads — but it's not the same DB you might guess.**

PebbleDB wins both: ~1M inserts/sec, ~1M reads/sec. An LSM `WriteBatch` is the closest thing to "memcpy with extra steps" any of these databases offer. There's no SQL parser, no transaction log replay, no concurrency control beyond the batch itself. Reads land on data already in the OS page cache, indexed by LSM block. *This is why every full-history blockchain client built since 2014 uses an LSM tree* — the property is structural, not coincidental.

**2. Mongo wins silver on inserts, last on reads, and it isn't close.**

100k blk/s in, 8k reads/s out. The reason is mechanical: bulk insert ships 1000 documents per round-trip, so a 100k workload is 100 protocol exchanges. Point reads are one round-trip each — 10,000 exchanges over the same network. Mongo's per-request overhead is higher than Postgres's, so under per-document `FindOne` it falls *behind* the relational competitor it normally beats on writes.

If you remember one thing from this benchmark, remember this: **pick your store by the dominant access pattern, not by the phase you benchmark first.** Mongo wins demos. Mongo loses production.

**3. SQLite and Postgres are within 5% on hash vs height.**

Both are B-tree-indexed lookups. The read path is identical except for which index page the engine starts at. Postgres pays a constant network round-trip cost on top, which dominates everything else, and is why it sits at ~16k reads/s regardless of which column we filter on. SQLite is in-process and runs ~2× faster.

**4. Disk size doesn't track row count linearly.**

Mongo's 13.6 MiB is the smallest because WiredTiger applies zlib compression by default. Pebble's 29.2 MiB includes ~8 MiB of secondary keyspace for the height index. SQLite stores raw with B-tree overhead. Postgres uses the most — MVCC tuple visibility headers, TOAST headers, and the WAL all cost real bytes.

Extrapolating naively to Bitcoin's ~900k blocks: ~260 MiB for Pebble (with the index), ~120 MiB for Mongo (compressed), ~380 MiB for Postgres — just for headers. The full chainstate is far larger because of the UTXO set, but the storage-engine choice is the same.

## Caveats

Things this benchmark does *not* tell you:

- **Reads are hot-cache.** The 100k working set fits in OS page cache after insertion. Cold-cache reads — say, a node booting and replaying the chain from disk — would be much slower across all four, and the gap between in-process (SQLite, Pebble) and over-network (Postgres, Mongo) would widen because the server clients pay a round-trip *plus* a disk seek on miss.
- **No concurrent writers.** Postgres and Mongo would handle 8 concurrent writers gracefully; SQLite would serialise; Pebble's `WriteBatch` is single-threaded by design.
- **No crash recovery.** Only Pebble's `pebble.Sync` forces an fsync. The others use their defaults, which means some inserts are still in OS buffers when the benchmark ends.
- **Small dataset.** 100k blocks is a B-tree depth of ~4 in Postgres. At 100M rows it's 6–7, and the order can change.

The benchmark is useful for *"is the order-of-magnitude difference what you'd expect"*. It is not a substitute for benchmarking your own workload.

## What our toy node picks

The C# toy node persists to SQLite, with `IBlockStore` deliberately narrow so the implementation could swap without touching anything else. SQLite was the right *teaching* choice: embedded, one-line schema, fast enough for the 1–10k block runs the toy actually does.

For a real client we'd pick Pebble or RocksDB without hesitation. The benchmark numbers explain why. SQLite is the teaching choice; an LSM is the production choice.

> Waku moved its store node from SQLite to Postgres around 2024. Their decision had nothing to do with insert speed and everything to do with operational tooling — replicas, backups, monitoring, schema migrations. That's the kind of consideration *no insert benchmark can capture*. Engine choice is workload first, operations second; benchmark numbers inform the first, your team's existing skills inform the second.
