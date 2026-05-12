package main

import (
	"context"
	"flag"
	"fmt"
	"math/rand"
	"os"
	"path/filepath"
	"strings"
	"time"
)

func main() {
	backend := flag.String("backend", "all", "sqlite|postgres|mongo|pebble|all (comma-separated)")
	count := flag.Int("count", 100_000, "number of blocks to insert")
	reads := flag.Int("reads", 10_000, "number of random reads per access pattern")
	outDir := flag.String("out", "out", "output directory for SQLite/Pebble files")
	flag.Parse()

	if err := os.MkdirAll(*outDir, 0o755); err != nil {
		fmt.Fprintln(os.Stderr, err)
		os.Exit(1)
	}

	fmt.Printf("Generating %d synthetic blocks…\n", *count)
	start := time.Now()
	blocks := GenerateBlocks(*count)
	fmt.Printf("    generated in %s (first hash %x…)\n\n",
		time.Since(start).Round(time.Millisecond), blocks[0].Hash[:8])

	// Stable RNG so every backend sees the same access pattern.
	r := rand.New(rand.NewSource(42))
	picks := make([]int, *reads)
	for i := range picks {
		picks[i] = r.Intn(len(blocks))
	}

	chosen := strings.Split(*backend, ",")
	if len(chosen) == 1 && chosen[0] == "all" {
		chosen = []string{"sqlite", "pebble", "postgres", "mongo"}
	}

	results := []runResult{}
	for _, name := range chosen {
		b := makeBackend(name, *outDir)
		if b == nil {
			fmt.Printf("[skip] unknown backend %q\n\n", name)
			continue
		}
		results = append(results, runOne(b, blocks, picks))
		fmt.Println()
	}

	printSummary(results, *count, *reads)
}

func makeBackend(name, outDir string) Backend {
	switch name {
	case "sqlite":
		return NewSqlite(filepath.Join(outDir, "bench.db"))
	case "pebble":
		return NewPebble(filepath.Join(outDir, "bench-pebble"))
	case "postgres":
		return NewPostgres()
	case "mongo":
		return NewMongo()
	}
	return nil
}

type runResult struct {
	name       string
	insertT    time.Duration
	insertRate float64
	hashT      time.Duration
	hashRate   float64
	heightT    time.Duration
	heightRate float64
	diskSize   int64
	err        error
}

func runOne(b Backend, blocks []Block, picks []int) runResult {
	r := runResult{name: b.Name()}
	fmt.Printf("=== %s ===\n", b.Name())

	ctx := context.Background()
	if err := b.Setup(ctx); err != nil {
		fmt.Printf("    setup: %v\n", err)
		r.err = err
		return r
	}
	defer b.Close()

	start := time.Now()
	if err := b.InsertAll(ctx, blocks); err != nil {
		fmt.Printf("    insert: %v\n", err)
		r.err = err
		return r
	}
	r.insertT = time.Since(start)
	r.insertRate = float64(len(blocks)) / r.insertT.Seconds()
	fmt.Printf("    insert  %d in %s   %.0f blk/s\n",
		len(blocks), r.insertT.Round(time.Millisecond), r.insertRate)

	// Random reads by hash
	start = time.Now()
	misses := 0
	for _, idx := range picks {
		blk, err := b.GetByHash(ctx, blocks[idx].Hash)
		if err != nil || blk == nil || blk.Height != blocks[idx].Height {
			misses++
		}
	}
	r.hashT = time.Since(start)
	r.hashRate = float64(len(picks)) / r.hashT.Seconds()
	fmt.Printf("    by hash  %d reads in %s   %.0f reads/s   %d misses\n",
		len(picks), r.hashT.Round(time.Millisecond), r.hashRate, misses)

	// Random reads by height
	start = time.Now()
	misses = 0
	for _, idx := range picks {
		blk, err := b.GetByHeight(ctx, blocks[idx].Height)
		if err != nil || blk == nil || blk.Hash != blocks[idx].Hash {
			misses++
		}
	}
	r.heightT = time.Since(start)
	r.heightRate = float64(len(picks)) / r.heightT.Seconds()
	fmt.Printf("    by height %d reads in %s   %.0f reads/s   %d misses\n",
		len(picks), r.heightT.Round(time.Millisecond), r.heightRate, misses)

	size, err := b.DiskSize(ctx)
	if err != nil {
		fmt.Printf("    disk size: %v\n", err)
	}
	r.diskSize = size
	fmt.Printf("    disk    %s\n", humanize(r.diskSize))
	return r
}

func printSummary(results []runResult, count, reads int) {
	fmt.Println("=== Summary ===")
	fmt.Printf("    %d blocks per run, %d random reads per access pattern\n\n", count, reads)

	hdr := fmt.Sprintf("    %-10s  %-9s  %-12s  %-12s  %-12s  %s",
		"backend", "insert", "insert/s", "byHash/s", "byHeight/s", "disk")
	sep := fmt.Sprintf("    %-10s  %-9s  %-12s  %-12s  %-12s  %s",
		"-------", "------", "--------", "--------", "----------", "----")
	fmt.Println(hdr)
	fmt.Println(sep)
	for _, r := range results {
		if r.err != nil {
			fmt.Printf("    %-10s  ERROR: %v\n", r.name, r.err)
			continue
		}
		fmt.Printf("    %-10s  %-9s  %-12.0f  %-12.0f  %-12.0f  %s\n",
			r.name,
			r.insertT.Round(time.Millisecond),
			r.insertRate,
			r.hashRate,
			r.heightRate,
			humanize(r.diskSize),
		)
	}
}

func humanize(n int64) string {
	switch {
	case n >= 1<<30:
		return fmt.Sprintf("%.2f GiB", float64(n)/(1<<30))
	case n >= 1<<20:
		return fmt.Sprintf("%.2f MiB", float64(n)/(1<<20))
	case n >= 1<<10:
		return fmt.Sprintf("%.1f KiB", float64(n)/(1<<10))
	default:
		return fmt.Sprintf("%d B", n)
	}
}
