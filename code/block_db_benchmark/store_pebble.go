package main

import (
	"context"
	"encoding/binary"
	"os"

	"github.com/cockroachdb/pebble"
)

type PebbleBackend struct {
	path string
	db   *pebble.DB
}

func NewPebble(path string) *PebbleBackend { return &PebbleBackend{path: path} }

func (b *PebbleBackend) Name() string { return "pebble" }

func (b *PebbleBackend) Setup(ctx context.Context) error {
	if err := os.RemoveAll(b.path); err != nil {
		return err
	}
	db, err := pebble.Open(b.path, &pebble.Options{})
	if err != nil {
		return err
	}
	b.db = db
	return nil
}

func (b *PebbleBackend) InsertAll(ctx context.Context, blocks []Block) error {
	batch := b.db.NewBatch()
	defer batch.Close()

	val := make([]byte, 4+32+32+4+4+4+4+2+4+80) // 170 bytes
	for i := range blocks {
		blk := &blocks[i]
		binary.LittleEndian.PutUint32(val[0:4], blk.Height)
		copy(val[4:36], blk.Prev[:])
		copy(val[36:68], blk.Merkle[:])
		binary.LittleEndian.PutUint32(val[68:72], blk.Timestamp)
		binary.LittleEndian.PutUint32(val[72:76], blk.Bits)
		binary.LittleEndian.PutUint32(val[76:80], blk.Nonce)
		binary.LittleEndian.PutUint32(val[80:84], uint32(blk.Version))
		binary.LittleEndian.PutUint16(val[84:86], blk.TxCount)
		binary.LittleEndian.PutUint32(val[86:90], blk.Size)
		copy(val[90:170], blk.Raw[:])

		if err := batch.Set(blk.Hash[:], val, nil); err != nil {
			return err
		}
	}
	return batch.Commit(pebble.Sync)
}

func (b *PebbleBackend) DiskSize(ctx context.Context) (int64, error) {
	return dirSize(b.path)
}

func (b *PebbleBackend) Close() error {
	if b.db != nil {
		return b.db.Close()
	}
	return nil
}
