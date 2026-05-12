package main

import (
	"context"
	"os"
	"path/filepath"
)

// Backend is the contract every storage implementation honours. The driver
// in main.go calls Setup, then times InsertAll, then asks for DiskSize.
type Backend interface {
	Name() string
	Setup(ctx context.Context) error
	InsertAll(ctx context.Context, blocks []Block) error
	GetByHash(ctx context.Context, hash [32]byte) (*Block, error)
	GetByHeight(ctx context.Context, height uint32) (*Block, error)
	DiskSize(ctx context.Context) (int64, error)
	Close() error
}

func dirSize(path string) (int64, error) {
	var size int64
	err := filepath.Walk(path, func(_ string, info os.FileInfo, err error) error {
		if err != nil {
			return err
		}
		if !info.IsDir() {
			size += info.Size()
		}
		return nil
	})
	return size, err
}
