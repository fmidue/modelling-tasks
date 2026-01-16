#!/bin/bash

# Checker script for .editorconfig compliance, meant for usage by Copilot.
# Ensures that files don't have trailing whitespace or missing final newlines.

set -e

echo "Checking for .editorconfig compliance..."

violations_found=0

# Check all relevant text files
while IFS= read -r -d '' file; do
  # Skip empty files (safe to ignore for these checks)
  [ -s "$file" ] || continue

  # echo "Checking: $file"

  # Check for trailing whitespace (spaces or tabs at end of lines)
  matches=$(grep -n '[[:space:]]$' "$file" || true)
  if [ -n "$matches" ]; then
    echo ""
    echo "ERROR: Found trailing whitespace in $file"
    echo "$matches" | head -5
    violations_found=$((violations_found + 1))
  fi

  # Check for missing final newline (except test/unit/** files per .editorconfig)
  if [[ ! "$file" =~ ^\.?/?test/unit/ ]]; then
    last_byte=$(tail -c1 "$file" | od -An -t u1 | tr -d '[:space:]')
    if [ "$last_byte" != "10" ]; then
      echo ""
      echo "ERROR: Missing final newline in $file"
      violations_found=$((violations_found + 1))
    fi
  fi

done < <(find . -type f \( -name "*.hs" -o -name "*.md" -o -name "*.yml" -o -name "*.yaml" \
  -o -name "*.cabal" -o -name "*.sh" -o -name "*.als" -o -name "*.tex" \) \
  -not -path "./.git/*" -not -path "*/.stack-work/*" -print0)

if [ $violations_found -gt 0 ]; then
  echo ""
  echo "❌ Found .editorconfig violation(s) in $violations_found files"
  echo "Please fix these issues before committing:"
  echo "- Remove trailing whitespace from files"
  echo "- Add final newlines to files (except test/unit/** files)"
  echo ""
  echo "You can use these commands to fix issues:"
  echo "  # Remove trailing whitespace:"
  echo "  sed -i 's/[[:space:]]*$//' filename"
  echo "  # Add final newline:"
  echo "  echo >> filename"
  exit 1
else
  echo ""
  echo "✅ All files comply with the .editorconfig settings relevant here."
  exit 0
fi
