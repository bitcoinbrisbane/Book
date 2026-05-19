#!/bin/bash

# Script to render LaTeX files to PDF
# Usage: ./scripts/render-pdf.sh <latex-file>
# Example: ./scripts/render-pdf.sh poker-formal-proof.tex

set -e  # Exit on error

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Function to print colored output
print_info() {
    echo -e "${BLUE}[INFO]${NC} $1"
}

print_success() {
    echo -e "${GREEN}[SUCCESS]${NC} $1"
}

print_warning() {
    echo -e "${YELLOW}[WARNING]${NC} $1"
}

print_error() {
    echo -e "${RED}[ERROR]${NC} $1"
}

# Check if filename provided
if [ $# -eq 0 ]; then
    print_error "No LaTeX file specified"
    echo "Usage: $0 <latex-file>"
    echo "Example: $0 poker-formal-proof.tex"
    exit 1
fi

LATEX_FILE="$1"

# Check if file exists
if [ ! -f "$LATEX_FILE" ]; then
    print_error "File not found: $LATEX_FILE"
    exit 1
fi

# Get the base name without extension
BASENAME=$(basename "$LATEX_FILE" .tex)
DIRNAME=$(dirname "$LATEX_FILE")

print_info "Rendering: $LATEX_FILE"
print_info "Output directory: $DIRNAME"

# Check if pdflatex is installed
if ! command -v pdflatex &> /dev/null; then
    print_error "pdflatex is not installed"
    echo ""
    echo "Please install LaTeX:"
    echo "  macOS:   brew install --cask mactex-no-gui"
    echo "           or: brew install basictex"
    echo "  Ubuntu:  sudo apt-get install texlive-full"
    echo "  Fedora:  sudo dnf install texlive-scheme-full"
    exit 1
fi

print_info "LaTeX distribution found: $(pdflatex --version | head -n1)"

# Change to the directory containing the LaTeX file
cd "$DIRNAME"

print_info "Running pdflatex (first pass)..."
pdflatex -interaction=nonstopmode -halt-on-error "$BASENAME.tex" > /dev/null 2>&1 || {
    print_error "First pdflatex pass failed"
    print_warning "Running with verbose output..."
    pdflatex -interaction=nonstopmode "$BASENAME.tex"
    exit 1
}

print_success "First pass completed"

# Check if bibliography exists and run bibtex if needed
if [ -f "$BASENAME.aux" ]; then
    # Check if there are citations in the aux file
    if grep -q "\\citation" "$BASENAME.aux" 2>/dev/null; then
        if command -v bibtex &> /dev/null; then
            print_info "Running bibtex..."
            bibtex "$BASENAME" > /dev/null 2>&1 || print_warning "Bibtex warnings (this is often normal)"
            print_success "Bibtex completed"
        else
            print_warning "bibtex not found, skipping bibliography generation"
        fi
    fi
fi

print_info "Running pdflatex (second pass for references)..."
pdflatex -interaction=nonstopmode -halt-on-error "$BASENAME.tex" > /dev/null 2>&1 || {
    print_error "Second pdflatex pass failed"
    exit 1
}

print_success "Second pass completed"

print_info "Running pdflatex (third pass for final references)..."
pdflatex -interaction=nonstopmode -halt-on-error "$BASENAME.tex" > /dev/null 2>&1 || {
    print_error "Third pdflatex pass failed"
    exit 1
}

print_success "Third pass completed"

# Check if PDF was created
if [ -f "$BASENAME.pdf" ]; then
    PDF_SIZE=$(du -h "$BASENAME.pdf" | cut -f1)
    print_success "PDF generated successfully: $BASENAME.pdf ($PDF_SIZE)"

    # Clean up auxiliary files
    print_info "Cleaning up auxiliary files..."
    rm -f "$BASENAME.aux" "$BASENAME.log" "$BASENAME.out" \
          "$BASENAME.toc" "$BASENAME.lof" "$BASENAME.lot" \
          "$BASENAME.bbl" "$BASENAME.blg" "$BASENAME.fls" \
          "$BASENAME.fdb_latexmk" "$BASENAME.synctex.gz"
    print_success "Cleanup completed"

    # Show PDF location
    echo ""
    echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
    print_success "PDF ready: $(pwd)/$BASENAME.pdf"
    echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"

    # Offer to open the PDF (macOS only)
    if [[ "$OSTYPE" == "darwin"* ]]; then
        echo ""
        read -p "Open PDF now? (y/n) " -n 1 -r
        echo
        if [[ $REPLY =~ ^[Yy]$ ]]; then
            open "$BASENAME.pdf"
        fi
    fi
else
    print_error "PDF generation failed - output file not created"
    exit 1
fi
