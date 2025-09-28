#!/usr/bin/env bash
# Checker script for .editorconfig compliance.
# Ensures that files comply with .editorconfig settings.

set -euo pipefail

echo "Checking for .editorconfig compliance..."

violations_found=0

# --- Helpers ---------------------------------------------------------------

has_cmd() { command -v "$1" >/dev/null 2>&1; }

# Very small pattern matcher just for a couple of paths we care about.
in_test_unit() { [[ "$1" == test/unit/* ]]; }

# Query .editorconfig via CLI if present; echo "unset" if not set.
# Falls back to the old hard-coded behavior when CLI is unavailable.
get_editorconfig_setting() {
  local file="$1" setting="$2"
  if has_cmd editorconfig; then
    # editorconfig CLI outputs key=value; we grep the key
    local val
    val="$(editorconfig "$file" 2>/dev/null | awk -F= -v k="$setting" '$1==k {print $2}')"
    if [[ -n "${val:-}" ]]; then
      echo "$val"
      return
    fi
    echo "unset"
  else
    case "$setting" in
      trim_trailing_whitespace)
        # Enforced everywhere in fallback
        echo "true"
        ;;
      insert_final_newline)
        if in_test_unit "$file"; then echo "unset"; else echo "true"; fi
        ;;
      end_of_line)
        if in_test_unit "$file"; then echo "unset"; else echo "lf"; fi
        ;;
      *) echo "unset" ;;
    esac
  fi
}

# Portable: does file end with newline?
has_final_newline() {
  # Returns 0 if file ends with \n, 1 otherwise
  # Works for non-empty files (we skip empty files elsewhere)
  local last_byte
  last_byte=$(tail -c 1 -- "$1" 2>/dev/null || true)
  # tail prints nothing when last byte isn't newline; compare literally
  [[ "$last_byte" == $'\n' ]]
}

# Any CR characters present?
has_crlf() {
  LC_ALL=C grep -q $'\r' -- "$1"
}

# Report up to 5 trailing-whitespace lines (spaces/tabs) ignoring a single CR.
report_trailing_whitespace() {
  awk '
    { sub(/\r$/,""); if ($0 ~ /[[:blank:]]+$/) { print NR ":" $0; c++ } }
    END { if (c>0) exit 0; else exit 1 }
  ' "$1" | head -5
}

has_trailing_whitespace() {
  awk '
    { sub(/\r$/,""); if ($0 ~ /[[:blank:]]+$/) { c=1; exit } }
    END { if (c) exit 0; else exit 1 }
  ' "$1"
}

# --- Main ------------------------------------------------------------------

# You can add more extensions here if your .editorconfig covers them.
mapfile -d '' files < <(find . -type f \
  \( -name "*.hs" -o -name "*.md" -o -name "*.yml" -o -name "*.yaml" \
     -name "*.cabal" -o -name "*.sh" -o -name "*.als" -o -name "*.tex" \) \
  -not -path "./.git/*" -not -path "./.stack-work/*" -print0)

for file in "${files[@]}"; do
  # Skip if missing or empty (empty files are fine w.r.t. final newline)
  [[ -f "$file" ]] || continue
  [[ -s "$file" ]] || continue

  echo "Checking: $file"

  # Determine settings (string values: true/false/lf/crlf/unset)
  trim_setting="$(get_editorconfig_setting "$file" "trim_trailing_whitespace")"
  newline_setting="$(get_editorconfig_setting "$file" "insert_final_newline")"
  eol_setting="$(get_editorconfig_setting "$file" "end_of_line")"

  # 1) EOL check (runs first to avoid TW noise due to CRs)
  if [[ "$eol_setting" == "lf" ]] && has_crlf "$file"; then
    echo "ERROR: Found CRLF line endings in $file (should be LF)"
    violations_found=$((violations_found + 1))
    # Keep going; trailing whitespace check below is CR-aware, so no double noise.
  fi

  # 2) Trailing whitespace (spaces/tabs before EOL; \r ignored for this check)
  if [[ "$trim_setting" == "true" ]] && has_trailing_whitespace "$file"; then
    echo "ERROR: Found trailing whitespace in $file"
    report_trailing_whitespace "$file" || true
    violations_found=$((violations_found + 1))
  fi

  # 3) Final newline
  if [[ "$newline_setting" == "true" ]] && ! has_final_newline "$file"; then
    echo "ERROR: Missing final newline in $file"
    violations_found=$((violations_found + 1))
  fi
done

if (( violations_found > 0 )); then
  echo
  echo "❌ Found $violations_found .editorconfig violation(s)"
  echo "Please fix these issues before committing:"
  echo "- Remove trailing whitespace from files"
  echo "- Add final newlines to files (except files where insert_final_newline is unset)"
  echo "- Ensure LF line endings where end_of_line = lf"
  echo
  echo "You can use these commands to fix issues:"
  echo "  # Remove trailing whitespace:"
  echo "  sed -i 's/[[:blank:]]\\+$//' filename"
  echo "  # Normalize to LF (strip CRs):"
  echo "  sed -i 's/\\r\\$//' filename"
  echo "  # Add final newline:"
  echo "  printf '\\n' >> filename"
  exit 1
else
  echo
  echo "✅ All files comply with .editorconfig settings"
  exit 0
fi
