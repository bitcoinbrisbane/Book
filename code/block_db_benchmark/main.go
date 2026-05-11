package main

import (
	"context"
	"flag"
	"fmt"
	"os"
	"path/filepath"
	"strings"
	"time"
)

func main() {
	backend := flag.String("backend", "all", "sqlite|postgres|mongo|pebble|all (comma-separated)")
	count := flag.Int("count", 100_000, "number of blocks to insert")
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
		results = append(results, runOne(b, blocks))
		fmt.Println()
	}

	printSummary(results, *count)
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
	name    string
	elapsed time.Duration
	rate    float64
	size    int64
	err     error
}

func runOne(b Backend, blocks []Block) runResult {
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
	r.elapsed = time.Since(start)
	r.rate = float64(len(blocks)) / r.elapsed.Seconds()

	size, err := b.DiskSize(ctx)
	if err != nil {
		fmt.Printf("    disk size: %v\n", err)
	}
	r.size = size

	fmt.Printf("    inserted %d in %s   %.0f blk/s   %s on disk\n",
		len(blocks), r.elapsed.Round(time.Millisecond), r.rate, humanize(r.size))
	return r
}

func printSummary(results []runResult, count int) {
	fmt.Println("=== Summary ===")
	fmt.Printf("    %d blocks per run\n\n", count)
	fmt.Printf("    %-10s  %-12s  %-12s  %s\n", "backend", "time", "blk/s", "disk")
	fmt.Printf("    %-10s  %-12s  %-12s  %s\n", "-------", "----", "-----", "----")
	for _, r := range results {
		if r.err != nil {
			fmt.Printf("    %-10s  ERROR: %v\n", r.name, r.err)
			continue
		}
		fmt.Printf("    %-10s  %-12s  %-12.0f  %s\n",
			r.name,
			r.elapsed.Round(time.Millisecond),
			r.rate,
			humanize(r.size),
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
