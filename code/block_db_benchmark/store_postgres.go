package main

import (
	"context"

	"github.com/jackc/pgx/v5"
	"github.com/jackc/pgx/v5/pgxpool"
)

const postgresDSN = "postgres://test:Test1234@localhost:5433/blocks_bench?sslmode=disable"

type PostgresBackend struct {
	pool *pgxpool.Pool
}

func NewPostgres() *PostgresBackend { return &PostgresBackend{} }

func (b *PostgresBackend) Name() string { return "postgres" }

func (b *PostgresBackend) Setup(ctx context.Context) error {
	pool, err := pgxpool.New(ctx, postgresDSN)
	if err != nil {
		return err
	}
	b.pool = pool

	if _, err := pool.Exec(ctx, "DROP TABLE IF EXISTS headers"); err != nil {
		return err
	}
	_, err = pool.Exec(ctx, `
		CREATE TABLE headers (
			hash      BYTEA PRIMARY KEY,
			height    INTEGER NOT NULL,
			prev      BYTEA NOT NULL,
			merkle    BYTEA NOT NULL,
			timestamp BIGINT NOT NULL,
			bits      BIGINT NOT NULL,
			nonce     BIGINT NOT NULL,
			version   INTEGER NOT NULL,
			tx_count  INTEGER NOT NULL,
			size      INTEGER NOT NULL,
			raw       BYTEA NOT NULL
		);
		CREATE INDEX idx_headers_height ON headers(height);
	`)
	return err
}

func (b *PostgresBackend) InsertAll(ctx context.Context, blocks []Block) error {
	tx, err := b.pool.Begin(ctx)
	if err != nil {
		return err
	}
	defer tx.Rollback(ctx)

	_, err = tx.Prepare(ctx, "ins_header", `
		INSERT INTO headers
		    (hash, height, prev, merkle, timestamp, bits, nonce, version, tx_count, size, raw)
		VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11)`)
	if err != nil {
		return err
	}

	for i := range blocks {
		blk := &blocks[i]
		if _, err := tx.Exec(ctx, "ins_header",
			blk.Hash[:], int32(blk.Height), blk.Prev[:], blk.Merkle[:],
			int64(blk.Timestamp), int64(blk.Bits), int64(blk.Nonce), blk.Version,
			int32(blk.TxCount), int32(blk.Size), blk.Raw[:],
		); err != nil {
			return err
		}
	}
	return tx.Commit(ctx)
}

func (b *PostgresBackend) DiskSize(ctx context.Context) (int64, error) {
	var size int64
	err := b.pool.QueryRow(ctx,
		"SELECT pg_database_size(current_database())").Scan(&size)
	return size, err
}

func (b *PostgresBackend) Close() error {
	if b.pool != nil {
		b.pool.Close()
	}
	return nil
}

// Keep pgx import used even if Prepare-by-name is the only consumer.
var _ = pgx.NamedArgs{}
