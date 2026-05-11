# Block DB benchmark

Inserts 100,000 synthetic block-header records into four databases — SQLite, PebbleDB, PostgreSQL, MongoDB — and reports wall-clock time, throughput, and on-disk size.

## Why this benchmark

The persistence-layer page in Chapter 3 made a few claims about how different storage engines behave under blockchain-shaped workloads. This is the matching experiment.

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
go run . -count 100000 -backend all
```

You can also pick a subset:

```bash
go run . -backend sqlite,pebble       # embedded only — no docker needed
go run . -backend postgres -count 10000
```

Output ends with a summary table. A representative run on a developer laptop (Ubuntu 26, Go 1.25, Docker 29):

```
=== Summary ===
    100000 blocks per run

    backend     time          blk/s         disk
    -------     ----          -----         ----
    sqlite      1.561s        64071         26.21 MiB
    pebble      63ms          1587778       19.65 MiB
    postgres    5.855s        17079         42.10 MiB
    mongo       964ms         103739        13.61 MiB
```

Your numbers will vary — CPU, fsync behaviour, and disk write cache all matter — but the relative shape is consistent:

- **PebbleDB** is fastest by a wide margin. An LSM-tree with one big `WriteBatch` is the closest thing to "memcpy" any of these databases offer.
- **Mongo** comes second on speed because `InsertMany` ships 1000 docs per round-trip with no SQL parsing.
- **SQLite** does well on a single transaction — within the same order of magnitude as Mongo despite being a B-tree.
- **Postgres** is slowest with our prepared-INSERT-in-tx strategy. `COPY FROM` would be ~5× faster but isn't what most application code does, so we measure the realistic pattern.

On disk, Mongo wins thanks to WiredTiger's zlib compression; Postgres uses the most because of MVCC overhead, TOAST headers, and the WAL.

## Methodology notes

Each backend uses its idiomatic "bulk insert N records" pattern, not its absolute fastest:

| Backend  | Strategy                                                            |
| -------- | ------------------------------------------------------------------- |
| SQLite   | single transaction, one prepared `INSERT` executed N times          |
| Postgres | single transaction, one prepared `INSERT` executed N times          |
| Mongo    | `InsertMany` in batches of 1000, unordered                          |
| Pebble   | one `WriteBatch`, all N puts, committed synchronously               |

We *deliberately* don't use `COPY` for Postgres (which would be ~5× faster) or `pebble.NoSync` for Pebble (also faster) — the goal is to compare what an application would actually write. The README's results table should be read as a directional indicator, not a benchmark you'd publish in a paper.

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

So: useful for "how do these databases feel" and "is the order-of-magnitude difference what you'd expect", not for production capacity planning. The Chapter 3 persistence page explains what each engine is *good at* — this benchmark is the matching small empirical check.
