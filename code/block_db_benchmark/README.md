# Block DB benchmark

Inserts 100,000 synthetic block-header records into four databases — SQLite, PebbleDB, PostgreSQL, MongoDB — then runs 10,000 random point reads against each (by primary-key hash, then by secondary-index height), and reports wall-clock time, throughput, and on-disk size for each phase.

## Why this benchmark

The persistence-layer page in Chapter 4 made a few claims about how different storage engines behave under blockchain-shaped workloads. This is the matching experiment.

- **SQLite** — embedded, single file. Default in many small projects.
- **PebbleDB** — embedded LSM-tree (Go), CockroachDB's replacement for RocksDB. Same shape as what Bitcoin Core uses for its chainstate.
- **PostgreSQL** — server, B-tree, full ACID. Default for general application backends.
- **MongoDB** — server, document store with WiredTiger LSM/B-tree hybrid. Default for "no schema" backends.

The schema is the same one our C# toy node persists: hash, height, prev, merkle root, timestamp, bits, nonce, version, tx_count, size, raw 80-byte header.

Blocks are *synthetic* — deterministically generated, with a real header structure and realistic byte sizes (~1–4 tx per block, double-SHA-256 chained hashes). This isolates the DB from network noise.

## Run

Start the dockerised databases (Postgres + Mongo); the embedded ones need nothing:

```bash
docker compose up -d
```

Then:

```bash
go mod tidy           # first run only
go run . -count 100000 -reads 10000 -backend all
```

You can also pick a subset:

```bash
go run . -backend sqlite,pebble       # embedded only — no docker needed
go run . -backend postgres -count 10000 -reads 5000
```

Flags:

| Flag       | Default | Meaning                                                          |
| ---------- | ------- | ---------------------------------------------------------------- |
| `-backend` | `all`   | Comma-separated subset: `sqlite,pebble,postgres,mongo`           |
| `-count`   | 100000  | Number of blocks to insert                                       |
| `-reads`   | 10000   | Number of random point reads per access pattern (hash + height)  |
| `-out`     | `out`   | Output directory for SQLite/Pebble files                         |

Output ends with a summary table. A representative run on a developer laptop (Ubuntu 26, Go 1.25, Docker 29):

```
=== Summary ===
    100000 blocks per run, 10000 random reads per access pattern

    backend     insert     insert/s      byHash/s      byHeight/s    disk
    -------     ------     --------      --------      ----------    ----
    sqlite      1.628s     61425         35067         33552         26.21 MiB
    pebble      120ms      832522        975627        546523        28.16 MiB
    postgres    5.795s     17257         16209         16819         42.17 MiB
    mongo       993ms      100666        8614          7721          13.61 MiB
```

Your numbers will vary — CPU, fsync behaviour, and disk write cache all matter — but the relative shape is consistent.

**On inserts**:
- **PebbleDB** wins by a wide margin. An LSM-tree with one big `WriteBatch` is the closest thing to "memcpy" any of these databases offer.
- **Mongo** comes second because `InsertMany` ships 1000 docs per round-trip with no SQL parsing.
- **SQLite** holds up on a single transaction.
- **Postgres** is slowest with our prepared-INSERT strategy. `COPY FROM` would be ~5× faster.

**On reads**:
- **PebbleDB** stays dominant: a single `Get` is a memcache hit + log-structured lookup. ~1M reads/s by hash.
- **SQLite** is the same order of magnitude for hash and height because both are B-tree-indexed; the read path is identical.
- **Postgres** is the same order of magnitude for hash and height (network round-trip dominates).
- **Mongo** is *slow* on per-document `FindOne` — slower than Postgres — despite winning on bulk insert. This is the inversion that gets people in trouble when they pick Mongo for a write-heavy benchmark and then deploy a read-heavy workload.

**On disk**:
- **Mongo** wins despite running a server, thanks to WiredTiger's zlib compression.
- **Pebble** grew by ~8 MiB from the previous run because we added a secondary `h/<height>` → hash index for height lookups — that's the cost of supporting two access patterns in a KV store.
- **Postgres** uses the most because of MVCC overhead, TOAST headers, and the WAL.

## Methodology notes

Each backend uses its idiomatic "bulk insert N records" pattern, not its absolute fastest:

| Backend  | Insert strategy                                                     | Hash lookup | Height lookup                          |
| -------- | ------------------------------------------------------------------- | ----------- | -------------------------------------- |
| SQLite   | single transaction, one prepared `INSERT` executed N times          | PK SELECT   | indexed SELECT on `height`             |
| Postgres | single transaction, one prepared `INSERT` executed N times          | PK SELECT   | indexed SELECT on `height`             |
| Mongo    | `InsertMany` in batches of 1000, unordered                          | `_id` find  | indexed `FindOne` on `height`          |
| Pebble   | one `WriteBatch`, all N puts, committed synchronously               | `Get` on `b/<hash>` | `Get` on `h/<height>` → `Get` on `b/<hash>` (two lookups) |

We *deliberately* don't use `COPY` for Postgres (which would be ~5× faster on inserts) or `pebble.NoSync` for Pebble (also faster on inserts) — the goal is to compare what an application would actually write. The README's results table should be read as a directional indicator, not a benchmark you'd publish in a paper.

Pebble's two-Get height lookup is what a real KV store does when you ask for a value via a non-primary key — there is no built-in "secondary index", so you build one as a separate keyspace. The extra hop is visible in the numbers (~half the rate of by-hash).

## What gets measured

- **Wall time** — the duration of `InsertAll`, *not* including connection setup or schema creation.
- **Throughput** — `count / elapsed` in blocks per second.
- **On-disk size**:
  - SQLite: `stat` on the `.db` file
  - Pebble: recursive size of the data directory
  - Postgres: `pg_database_size(current_database())`
  - Mongo: `dbStats.storageSize`

## Teardown

```bash
docker compose down -v       # stop containers, remove volumes
rm -rf out/                  # clean SQLite + Pebble artifacts
```

## What this benchmark does not measure

- **Reads.** This is insert-only. Real blockchain workloads are also read-heavy (UTXO lookups dominate).
- **Concurrent writers.** All writes are single-threaded.
- **Crash recovery.** Pebble's `pebble.Sync` is honest about durability; SQLite uses default journal mode; nobody is power-cut tested.
- **Long-tail growth.** 100k blocks fit comfortably in RAM/cache for every backend. Real blockchain workloads have hundreds of millions of rows where index height matters.

So: useful for "how do these databases feel" and "is the order-of-magnitude difference what you'd expect", not for production capacity planning. The Chapter 4 persistence page explains what each engine is *good at* — this benchmark is the matching small empirical check.
