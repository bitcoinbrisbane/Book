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

func (b *SqliteBackend) GetByHash(ctx context.Context, hash [32]byte) (*Block, error) {
	return b.scanOne(ctx,
		`SELECT hash, height, prev, merkle, timestamp, bits, nonce, version, tx_count, size, raw
		 FROM headers WHERE hash = ?`,
		hash[:])
}

func (b *SqliteBackend) GetByHeight(ctx context.Context, height uint32) (*Block, error) {
	return b.scanOne(ctx,
		`SELECT hash, height, prev, merkle, timestamp, bits, nonce, version, tx_count, size, raw
		 FROM headers WHERE height = ?`,
		int64(height))
}

func (b *SqliteBackend) scanOne(ctx context.Context, query string, arg any) (*Block, error) {
	var (
		blk                      Block
		hash, prev, merkle, raw  []byte
		height, ts, bits, nonce  int64
		version, txCount, sizeFn int64
	)
	row := b.db.QueryRowContext(ctx, query, arg)
	if err := row.Scan(&hash, &height, &prev, &merkle, &ts, &bits, &nonce, &version, &txCount, &sizeFn, &raw); err != nil {
		return nil, err
	}
	copy(blk.Hash[:], hash)
	blk.Height = uint32(height)
	copy(blk.Prev[:], prev)
	copy(blk.Merkle[:], merkle)
	blk.Timestamp = uint32(ts)
	blk.Bits = uint32(bits)
	blk.Nonce = uint32(nonce)
	blk.Version = int32(version)
	blk.TxCount = uint16(txCount)
	blk.Size = uint32(sizeFn)
	copy(blk.Raw[:], raw)
	return &blk, nil
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
