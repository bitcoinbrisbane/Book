package main

import (
	"context"
	"database/sql"
	"os"

	_ "modernc.org/sqlite"
)

type SqliteBackend struct {
	path string
	db   *sql.DB
}

func NewSqlite(path string) *SqliteBackend { return &SqliteBackend{path: path} }

func (b *SqliteBackend) Name() string { return "sqlite" }

func (b *SqliteBackend) Setup(ctx context.Context) error {
	_ = os.Remove(b.path)
	_ = os.Remove(b.path + "-journal")
	_ = os.Remove(b.path + "-wal")
	_ = os.Remove(b.path + "-shm")

	db, err := sql.Open("sqlite", b.path)
	if err != nil {
		return err
	}
	b.db = db

	_, err = db.ExecContext(ctx, `
		CREATE TABLE headers (
			hash      BLOB PRIMARY KEY,
			height    INTEGER NOT NULL,
			prev      BLOB NOT NULL,
			merkle    BLOB NOT NULL,
			timestamp INTEGER NOT NULL,
			bits      INTEGER NOT NULL,
			nonce     INTEGER NOT NULL,
			version   INTEGER NOT NULL,
			tx_count  INTEGER NOT NULL,
			size      INTEGER NOT NULL,
			raw       BLOB NOT NULL
		);
		CREATE INDEX idx_headers_height ON headers(height);
	`)
	return err
}

func (b *SqliteBackend) InsertAll(ctx context.Context, blocks []Block) error {
	tx, err := b.db.BeginTx(ctx, nil)
	if err != nil {
		return err
	}
	defer tx.Rollback()

	stmt, err := tx.PrepareContext(ctx, `
		INSERT INTO headers
		    (hash, height, prev, merkle, timestamp, bits, nonce, version, tx_count, size, raw)
		VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)`)
	if err != nil {
		return err
	}
	defer stmt.Close()

	for i := range blocks {
		blk := &blocks[i]
		if _, err := stmt.ExecContext(ctx,
			blk.Hash[:], blk.Height, blk.Prev[:], blk.Merkle[:],
			blk.Timestamp, blk.Bits, blk.Nonce, blk.Version,
			blk.TxCount, blk.Size, blk.Raw[:],
		); err != nil {
			return err
		}
	}
	return tx.Commit()
}

func (b *SqliteBackend) DiskSize(ctx context.Context) (int64, error) {
	info, err := os.Stat(b.path)
	if err != nil {
		return 0, err
	}
	return info.Size(), nil
}

func (b *SqliteBackend) Close() error {
	if b.db != nil {
		return b.db.Close()
	}
	return nil
}
