#!/bin/bash

# Word Count Script for Book
# Counts all words in markdown files across the book

# Colors for output
GREEN='\033[0;32m'
BLUE='\033[0;34m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Get the book directory (parent of scripts directory)
BOOK_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"

echo -e "${BLUE}========================================${NC}"
echo -e "${BLUE}    Book Word Count Report${NC}"
echo -e "${BLUE}========================================${NC}"
echo ""

# Initialize counters
total_words=0
total_files=0

# Find all markdown files and count words (excluding code, src, and node_modules directories)
while IFS= read -r file; do
    # Get word count for the file
    words=$(wc -w < "$file")

    # Add to totals
    total_words=$((total_words + words))
    total_files=$((total_files + 1))

    # Get relative path for cleaner output
    rel_path="${file#$BOOK_DIR/}"

    # Print file info
    printf "${GREEN}%-60s${NC} %8d words\n" "$rel_path" "$words"

done < <(find "$BOOK_DIR" -name "*.md" -type f -not -path "*/code/*" -not -path "*/src/*" -not -path "*/node_modules/*" | sort)

echo ""
echo -e "${BLUE}========================================${NC}"
echo -e "${YELLOW}Total Files:${NC} $total_files"
echo -e "${YELLOW}Total Words:${NC} $total_words"
echo -e "${BLUE}========================================${NC}"
