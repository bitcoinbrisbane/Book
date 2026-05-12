package main

import (
	"context"
	"encoding/binary"
	"errors"
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

// Keyspace layout:
//   b/<32-byte hash>          -> 170-byte encoded block value
//   h/<4-byte big-endian h>   -> 32-byte hash (secondary index for height lookup)
//
// Big-endian height keeps height ranges contiguous on disk, in case we
// later want to range-scan.

func blockKey(hash [32]byte) []byte {
	k := make([]byte, 1+32)
	k[0] = 'b'
	copy(k[1:], hash[:])
	return k
}

func heightKey(h uint32) []byte {
	k := make([]byte, 1+4)
	k[0] = 'h'
	binary.BigEndian.PutUint32(k[1:], h)
	return k
}

func encodeBlockValue(blk *Block) []byte {
	val := make([]byte, 4+32+32+4+4+4+4+2+4+80) // 170
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
	return val
}

func decodeBlockValue(hash [32]byte, val []byte) (*Block, error) {
	if len(val) != 170 {
		return nil, errors.New("pebble: bad block value length")
	}
	var blk Block
	blk.Hash = hash
	blk.Height = binary.LittleEndian.Uint32(val[0:4])
	copy(blk.Prev[:], val[4:36])
	copy(blk.Merkle[:], val[36:68])
	blk.Timestamp = binary.LittleEndian.Uint32(val[68:72])
	blk.Bits = binary.LittleEndian.Uint32(val[72:76])
	blk.Nonce = binary.LittleEndian.Uint32(val[76:80])
	blk.Version = int32(binary.LittleEndian.Uint32(val[80:84]))
	blk.TxCount = binary.LittleEndian.Uint16(val[84:86])
	blk.Size = binary.LittleEndian.Uint32(val[86:90])
	copy(blk.Raw[:], val[90:170])
	return &blk, nil
}

func (b *PebbleBackend) InsertAll(ctx context.Context, blocks []Block) error {
	batch := b.db.NewBatch()
	defer batch.Close()

	for i := range blocks {
		blk := &blocks[i]
		val := encodeBlockValue(blk)
		if err := batch.Set(blockKey(blk.Hash), val, nil); err != nil {
			return err
		}
		if err := batch.Set(heightKey(blk.Height), blk.Hash[:], nil); err != nil {
			return err
		}
	}
	return batch.Commit(pebble.Sync)
}

func (b *PebbleBackend) GetByHash(ctx context.Context, hash [32]byte) (*Block, error) {
	val, closer, err := b.db.Get(blockKey(hash))
	if err != nil {
		return nil, err
	}
	defer closer.Close()
	return decodeBlockValue(hash, val)
}

func (b *PebbleBackend) GetByHeight(ctx context.Context, height uint32) (*Block, error) {
	hashBytes, closer, err := b.db.Get(heightKey(height))
	if err != nil {
		return nil, err
	}
	var hash [32]byte
	copy(hash[:], hashBytes)
	closer.Close()
	return b.GetByHash(ctx, hash)
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
