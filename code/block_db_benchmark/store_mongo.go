package main

import (
	"context"
	"fmt"

	"go.mongodb.org/mongo-driver/bson"
	"go.mongodb.org/mongo-driver/bson/primitive"
	"go.mongodb.org/mongo-driver/mongo"
	"go.mongodb.org/mongo-driver/mongo/options"
)

const (
	mongoURI = "mongodb://localhost:27017"
	mongoDB  = "blocks_bench"
)

type MongoBackend struct {
	client *mongo.Client
	coll   *mongo.Collection
}

func NewMongo() *MongoBackend { return &MongoBackend{} }

func (b *MongoBackend) Name() string { return "mongo" }

func (b *MongoBackend) Setup(ctx context.Context) error {
	client, err := mongo.Connect(ctx, options.Client().ApplyURI(mongoURI))
	if err != nil {
		return err
	}
	b.client = client

	db := client.Database(mongoDB)
	if err := db.Drop(ctx); err != nil {
		return err
	}
	b.coll = db.Collection("headers")

	_, err = b.coll.Indexes().CreateOne(ctx, mongo.IndexModel{
		Keys: bson.D{{Key: "height", Value: 1}},
	})
	return err
}

func (b *MongoBackend) InsertAll(ctx context.Context, blocks []Block) error {
	const batch = 1000
	docs := make([]interface{}, 0, batch)

	for i := range blocks {
		blk := &blocks[i]
		docs = append(docs, bson.M{
			"_id":       blk.Hash[:],
			"height":    blk.Height,
			"prev":      blk.Prev[:],
			"merkle":    blk.Merkle[:],
			"timestamp": blk.Timestamp,
			"bits":      blk.Bits,
			"nonce":     blk.Nonce,
			"version":   blk.Version,
			"tx_count":  blk.TxCount,
			"size":      blk.Size,
			"raw":       blk.Raw[:],
		})
		if len(docs) == batch || i == len(blocks)-1 {
			if _, err := b.coll.InsertMany(ctx, docs, options.InsertMany().SetOrdered(false)); err != nil {
				return err
			}
			docs = docs[:0]
		}
	}
	return nil
}

func (b *MongoBackend) GetByHash(ctx context.Context, hash [32]byte) (*Block, error) {
	return b.findOne(ctx, bson.M{"_id": hash[:]})
}

func (b *MongoBackend) GetByHeight(ctx context.Context, height uint32) (*Block, error) {
	return b.findOne(ctx, bson.M{"height": int64(height)})
}

func (b *MongoBackend) findOne(ctx context.Context, filter bson.M) (*Block, error) {
	var doc bson.M
	if err := b.coll.FindOne(ctx, filter).Decode(&doc); err != nil {
		return nil, err
	}
	var blk Block
	if v, ok := doc["_id"].(primitive.Binary); ok {
		copy(blk.Hash[:], v.Data)
	}
	blk.Height = bsonAsUint32(doc["height"])
	if v, ok := doc["prev"].(primitive.Binary); ok {
		copy(blk.Prev[:], v.Data)
	}
	if v, ok := doc["merkle"].(primitive.Binary); ok {
		copy(blk.Merkle[:], v.Data)
	}
	blk.Timestamp = bsonAsUint32(doc["timestamp"])
	blk.Bits = bsonAsUint32(doc["bits"])
	blk.Nonce = bsonAsUint32(doc["nonce"])
	blk.Version = int32(bsonAsInt64(doc["version"]))
	blk.TxCount = uint16(bsonAsUint32(doc["tx_count"]))
	blk.Size = bsonAsUint32(doc["size"])
	if v, ok := doc["raw"].(primitive.Binary); ok {
		copy(blk.Raw[:], v.Data)
	}
	return &blk, nil
}

func bsonAsUint32(v interface{}) uint32 {
	switch n := v.(type) {
	case int32:
		return uint32(n)
	case int64:
		return uint32(n)
	case float64:
		return uint32(n)
	}
	return 0
}

func bsonAsInt64(v interface{}) int64 {
	switch n := v.(type) {
	case int32:
		return int64(n)
	case int64:
		return n
	case float64:
		return int64(n)
	}
	return 0
}

func (b *MongoBackend) DiskSize(ctx context.Context) (int64, error) {
	// Force a checkpoint so WiredTiger flushes the recent writes to disk
	// before we read storageSize — otherwise dbStats reports a fraction of
	// the actual on-disk footprint until the next periodic flush (~60s).
	_ = b.client.Database("admin").
		RunCommand(ctx, bson.D{{Key: "fsync", Value: 1}}).Err()

	var result bson.M
	if err := b.client.Database(mongoDB).
		RunCommand(ctx, bson.D{{Key: "dbStats", Value: 1}}).Decode(&result); err != nil {
		return 0, err
	}
	v, ok := result["storageSize"]
	if !ok {
		return 0, fmt.Errorf("storageSize not in dbStats response")
	}
	switch n := v.(type) {
	case int64:
		return n, nil
	case int32:
		return int64(n), nil
	case float64:
		return int64(n), nil
	default:
		return 0, fmt.Errorf("storageSize unexpected type %T", v)
	}
}

func (b *MongoBackend) Close() error {
	if b.client != nil {
		return b.client.Disconnect(context.Background())
	}
	return nil
}
