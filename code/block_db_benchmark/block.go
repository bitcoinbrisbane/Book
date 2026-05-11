package main

import (
	"crypto/sha256"
	"encoding/binary"
)

// Block is a synthetic block-shaped record — same fields the C# toy node
// persists, generated deterministically so two runs produce identical input.
type Block struct {
	Hash      [32]byte
	Height    uint32
	Prev      [32]byte
	Merkle    [32]byte
	Timestamp uint32
	Bits      uint32
	Nonce     uint32
	Version   int32
	TxCount   uint16
	Size      uint32
	Raw       [80]byte
}

func GenerateBlocks(n int) []Block {
	blocks := make([]Block, n)
	var prev [32]byte

	for i := 0; i < n; i++ {
		var b Block
		b.Height = uint32(i + 1)
		b.Version = 1
		b.Prev = prev

		// Deterministic merkle root: sha256 of the height.
		var idxBuf [4]byte
		binary.LittleEndian.PutUint32(idxBuf[:], b.Height)
		b.Merkle = sha256.Sum256(idxBuf[:])

		b.Timestamp = 1231469665 + b.Height*600 // ~10 min/block from genesis time
		b.Bits = 0x1d00ffff
		b.Nonce = mixNonce(b.Height)

		// Realistic tx_count & size shape for the first ~150k blocks: 1–4 txs each.
		b.TxCount = uint16(1 + (b.Height % 4))
		b.Size = 80 + uint32(b.TxCount)*250 // ~250 B per tx, a typical legacy size

		binary.LittleEndian.PutUint32(b.Raw[0:4], uint32(b.Version))
		copy(b.Raw[4:36], b.Prev[:])
		copy(b.Raw[36:68], b.Merkle[:])
		binary.LittleEndian.PutUint32(b.Raw[68:72], b.Timestamp)
		binary.LittleEndian.PutUint32(b.Raw[72:76], b.Bits)
		binary.LittleEndian.PutUint32(b.Raw[76:80], b.Nonce)

		h1 := sha256.Sum256(b.Raw[:])
		h2 := sha256.Sum256(h1[:])
		b.Hash = h2

		prev = b.Hash
		blocks[i] = b
	}
	return blocks
}

func mixNonce(h uint32) uint32 {
	x := h * 2654435761 // Knuth's multiplicative hash
	return x ^ (x >> 16)
}
