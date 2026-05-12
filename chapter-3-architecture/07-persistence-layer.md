## Persistence layer

Our blockchain will need to save data — eventually a lot of it. Every committed block becomes permanent state that future nodes have to read back during validation. So before we write a line of code, we need to decide *how* that state is stored.

The good news is that the choice is well-trodden. Real Bitcoin and Ethereum clients have made it, written about it, and benchmarked it for years. The bad news is that the choice is *not obvious* — each engine has trade-offs that only show up under load.

## Four engines worth knowing

A blockchain's persistence layer typically picks from four families. Each shows up somewhere in production:

- **SQLite** — embedded, single file, full SQL, B-tree indexes. Default in many small projects because there's nothing to install. Surprisingly fast with a single transaction wrapping many inserts.
- **PebbleDB** (or **RocksDB**) — embedded LSM-tree. The shape Bitcoin Core uses for its chainstate (LevelDB, RocksDB's predecessor); the shape Cockroach, TiKV, and Ethereum's Geth use for almost all storage. Optimised for write-heavy append-mostly workloads, which is exactly what a chain is.
- **PostgreSQL** — server, B-tree, full ACID, the most mature relational database. Used by indexers (TheGraph), some Ethereum clients (Erigon supports a Postgres backend), and Waku for its store node.
- **MongoDB** — server, document store, schema-flexible. Common when blockchain data has to be served to a web frontend with arbitrary shapes — block explorers and analytics tools often put a Mongo collection in front of the chain.

There's no objectively right answer. The right answer is workload-driven, which is what the benchmark in the next section is for.

> A note on **Redis** — sometimes mentioned alongside these. Redis is in-memory by default and durability is opt-in via AOF or RDB snapshots. For an application *cache* in front of a blockchain client, it's excellent. As the canonical store of chain state, it's the wrong shape — losing the mempool isn't catastrophic, losing the UTXO set is.

## Permissioned vs permissionless

A brief aside that crosses chapters. Blockchains are sometimes categorised as **permissioned** or **permissionless** — *permission* here meaning the authority to read or write state. A permissioned chain (Hyperledger Fabric, Quorum) has a known set of validators and may run on infrastructure that's privately operated. A permissionless chain (Bitcoin, Ethereum) doesn't.

This matters for persistence because permissioned operators sometimes pick traditional RDBMS backends — Postgres, even Oracle — for the operational tooling (backups, replicas, audit logs) they already know. Permissionless clients almost always pick embedded LSM stores, because they're optimising for single-process write throughput and don't need DBA tooling.

## Comparing the four with real workloads

To make any of these claims concrete we wrote a small benchmark — [`code/block_db_benchmark`](../code/block_db_benchmark/) — that inserts 100,000 synthetic block-header records into each engine and reports time, throughput, and on-disk size. The schema is the same one our C# toy node uses: `hash`, `height`, `prev`, `merkle`, `timestamp`, `bits`, `nonce`, `version`, `tx_count`, `size`, and the raw 80-byte header. About 200 bytes per row, ~20 MiB of raw input data.

It's written in Go because PebbleDB is pure Go and that lets the whole benchmark — embedded *and* server backends — compile without any C toolchain. Mongo and Postgres run in containers; SQLite and Pebble run in-process.

Stack:

| Component | Library                                |
| --------- | -------------------------------------- |
| SQLite    | `modernc.org/sqlite` (pure-Go SQLite)  |
| PebbleDB  | `github.com/cockroachdb/pebble`        |
| PostgreSQL| `github.com/jackc/pgx/v5`              |
| MongoDB   | `go.mongodb.org/mongo-driver`          |

Each backend implements the same `Backend` interface — `Setup`, `InsertAll`, `DiskSize`, `Close` — so the driver code is identical. Each one picks its idiomatic *bulk insert* path:

| Backend  | Bulk-insert strategy                                                 |
| -------- | -------------------------------------------------------------------- |
| SQLite   | one transaction, one prepared `INSERT`, executed N times             |
| Postgres | one transaction, one prepared `INSERT`, executed N times             |
| Mongo    | `InsertMany` in batches of 1000, unordered                           |
| Pebble   | one `WriteBatch`, all N puts, committed synchronously                |

Postgres could be ~5× faster with `COPY FROM`, and Pebble could be faster with `pebble.NoSync`. We deliberately picked the patterns an *ordinary application* would use, not the absolute fastest. The benchmark is meant to feel like the code an app developer would actually write.

## Running it

```bash
cd code/block_db_benchmark
docker compose up -d        # Postgres + Mongo
go mod tidy                 # first run only
go run . -count 100000 -backend all
```

## Results

A representative run on a developer laptop (8-core, NVMe):

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

Your numbers will vary by CPU, fsync behaviour, and disk write cache — but the relative shape is the interesting story:

**Speed**: PebbleDB is two orders of magnitude faster than Postgres for this workload. An LSM `WriteBatch` is the closest thing to "memcpy with extra steps" any of these databases offer — there's no SQL parser, no transaction log replay path, no concurrency control beyond the batch itself. Mongo's `InsertMany` is the next fastest because it ships 1000 documents per round-trip with no per-row parsing. SQLite holds up surprisingly well on a single transaction. Postgres is slowest because each row pays for MVCC, the WAL, and the SQL parser.

**Disk**: Mongo wins on storage despite being a server because WiredTiger applies zlib compression by default. Pebble compresses too, with Snappy. SQLite stores raw with B-tree overhead. Postgres uses ~2× the raw data size because of TOAST headers, MVCC tuple visibility, and the WAL.

**Order of magnitude**: a full Bitcoin chain is ~900k blocks. Extrapolating from this 100k benchmark — naively — you'd expect roughly 240 MiB for Pebble, 380 MiB for Mongo, and 400 MiB for Postgres just for *headers*. The real Bitcoin chainstate is much larger than that because of the UTXO set, not the headers, but the storage-engine choice is the same one.

## What the benchmark doesn't tell you

Equally important to flag:

- **Reads aren't measured.** Real blockchain workloads are also read-heavy — every block validation walks the UTXO set. Pebble's read-amplification on an LSM is different from Postgres's B-tree behaviour, and matters more than insert speed for normal operation.
- **No concurrent writers.** All writes are single-threaded. Postgres and Mongo would handle 8 concurrent writers gracefully; SQLite's WAL would serialise; Pebble's `WriteBatch` is single-threaded by design.
- **No crash recovery.** None of these were power-cut tested. Pebble's `pebble.Sync` is the only one explicitly forcing fsync; the others rely on their defaults.
- **No long-tail growth.** 100k blocks fit comfortably in RAM/cache for every backend. At 100M rows index depth matters, and Postgres often pulls ahead of NoSQL stores on point lookups.

So: the benchmark is useful for *"how do these databases feel"* and *"is the order-of-magnitude difference what you'd expect"*. It is **not** sufficient for production capacity planning. Real evaluation needs the workload, not just the schema.

## Putting it in our toy node

Our C# toy node picks SQLite, with the `IBlockStore` interface deliberately narrow enough that swapping to Pebble or Postgres would be a one-class change. We chose SQLite because:

- It's embedded — no daemon, no setup, no docker.
- It has a one-line schema we can paste into the chapter.
- It's fast enough for the IBD volumes the toy actually does (1000–10000 blocks).

For a real client we'd pick Pebble or RocksDB without hesitation. SQLite is the *teaching* choice; Pebble is the *production* choice. The benchmark numbers explain why.

## Operational ceremony

Once the databases are running in containers, a few commands are worth knowing:

```bash
# Inspect what's in Postgres
docker exec blockbench-postgres \
    psql -U test -d blocks_bench -c "SELECT count(*) FROM headers;"

# Inspect what's in Mongo
docker exec blockbench-mongo mongosh blocks_bench \
    --quiet --eval 'db.headers.count()'

# Stop containers and remove volumes
docker compose down -v

# Clean up embedded outputs
rm -rf out/
```

> **Note**: Waku is using Postgres now (their store node moved off SQLite around 2024). Their choice has nothing to do with insert speed and everything to do with operational tooling — replicas, backups, monitoring — which is the kind of consideration *no insert benchmark can capture*.
