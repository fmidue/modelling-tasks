#!/bin/bash

# Checker script for .editorconfig compliance, meant for usage by Copilot.
# Ensures that files comply with .editorconfig settings

set -e

echo "Checking for .editorconfig compliance..."

violations_found=0

# Function to check if a file path matches a pattern
matches_pattern() {
  local file="$1"
  local pattern="$2"

  # Convert glob pattern to regex
  # This is a simplified conversion for the patterns we use
  case "$pattern" in
  "*.als") [[ "$file" =~ \.als$ ]] ;;
  "*.hs") [[ "$file" =~ \.hs$ ]] ;;
  "*.md") [[ "$file" =~ \.md$ ]] ;;
  "test/unit/**") [[ "$file" =~ ^test/unit/ ]] ;;
  "example/**") [[ "$file" =~ ^example/ ]] ;;
  "**.{yml,yaml,md,tex,cabal}") [[ "$file" =~ \.(yml|yaml|md|tex|cabal)$ ]] ;;
  "*") return 0 ;;
  esac
}

# Function to get .editorconfig setting for a file
get_editorconfig_setting() {
  local file="$1"
  local setting="$2"

  # Default values for all files
  case "$setting" in
  "trim_trailing_whitespace")
    # trim_trailing_whitespace is NOT unset for test/unit/** in .editorconfig
    echo "true"
    ;;
  "insert_final_newline")
    if matches_pattern "$file" "test/unit/**"; then
      echo "unset"
    else
      echo "true"
    fi
    ;;
  "end_of_line")
    if matches_pattern "$file" "test/unit/**"; then
      echo "unset"
    else
      echo "lf"
    fi
    ;;
  esac
}

# Check all relevant files
while IFS= read -r -d '' file; do
  # Skip if file doesn't exist (could be deleted)
  [ -f "$file" ] || continue

  # Skip empty files for newline checks
  [ -s "$file" ] || continue

  echo "Checking: $file"

  # Check trailing whitespace
  trim_setting=$(get_editorconfig_setting "$file" "trim_trailing_whitespace")
  if [ "$trim_setting" = "true" ]; then
    if grep -q '[[:space:]]$' "$file"; then
      echo "ERROR: Found trailing whitespace in $file"
      grep -n '[[:space:]]$' "$file" | head -5
      violations_found=$((violations_found + 1))
    fi
  fi

  # Check final newline
  newline_setting=$(get_editorconfig_setting "$file" "insert_final_newline")
  if [ "$newline_setting" = "true" ]; then
    if [ "$(tail -c1 "$file" | wc -l)" -eq 0 ]; then
      echo "ERROR: Missing final newline in $file"
      violations_found=$((violations_found + 1))
    fi
  fi

  # Check line endings (only for files that should have LF)
  eol_setting=$(get_editorconfig_setting "$file" "end_of_line")
  if [ "$eol_setting" = "lf" ]; then
    if grep -q $'\r' "$file"; then
      echo "ERROR: Found CRLF line endings in $file (should be LF)"
      violations_found=$((violations_found + 1))
    fi
  fi

done < <(find . -type f \( -name "*.hs" -o -name "*.md" -o -name "*.yml" -o -name "*.yaml" \
    -o -name "*.cabal" -o -name "*.sh" -o -name "*.als" -o -name "*.tex" \) \
    -not -path "./.git/*" -not -path "./.stack-work/*" -print0)

if [ $violations_found -gt 0 ]; then
    echo ""
    echo "❌ Found $violations_found .editorconfig violation(s)"
    echo "Please fix these issues before committing:"
    echo "- Remove trailing whitespace from files"
    echo "- Add final newlines to files (except test/unit/** files)"
    echo "- Ensure LF line endings (except test/unit/** files)"
    echo ""
    echo "You can use these commands to fix issues:"
    echo "  # Remove trailing whitespace:"
    echo "  sed -i 's/[[:space:]]*$//' filename"
    echo "  # Add final newline:"
    echo "  echo >> filename"
    exit 1
else
    echo ""
    echo "✅ All files comply with .editorconfig settings"
    exit 0
fi
