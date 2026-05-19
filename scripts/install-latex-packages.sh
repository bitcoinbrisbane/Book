#!/bin/bash

# Script to install required LaTeX packages for the poker formal proof paper
# This requires sudo permissions

set -e

echo "Installing required LaTeX packages..."
echo "This will require your password for sudo access."
echo ""

# Update tlmgr
echo "Updating TeX Live Manager..."
sudo /Library/TeX/texbin/tlmgr update --self

# Install required packages
echo ""
echo "Installing packages: algorithms, enumitem..."
sudo /Library/TeX/texbin/tlmgr install algorithms enumitem

echo ""
echo "✓ All LaTeX packages installed successfully!"
echo ""
echo "You can now run: ./scripts/render-pdf.sh poker-formal-proof.tex"
