#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "$0")/../.." && pwd)"
SOURCE="$ROOT/portable/scripts/bundle-r.sh"
TMP="$(mktemp -d)"
trap 'rm -rf "$TMP"' EXIT

# Load only the function under test so bundling does not start.
sed -n '/^validate_r_version() {/,/^}/p' "$SOURCE" > "$TMP/function.sh"
# shellcheck source=/dev/null
source "$TMP/function.sh"
R_VERSION=4.5.2
PLATFORM=linux

make_rscript() {
  local body="$1"
  cat > "$TMP/Rscript" <<EOF
#!/usr/bin/env bash
$body
EOF
  chmod +x "$TMP/Rscript"
}

assert_failure() {
  local expected="$1" output
  if output="$(validate_r_version "$TMP/Rscript" 2>&1)"; then
    echo "Expected version validation to fail." >&2
    exit 1
  fi
  [[ "$output" == *"$expected"* ]] || {
    printf 'Expected output containing <%s>, got:\n%s\n' "$expected" "$output" >&2
    exit 1
  }
}

make_rscript "printf '  4.5.2\\n'"
validate_r_version "$TMP/Rscript"

make_rscript "echo 'stdout diagnostic'; echo 'loader diagnostic' >&2; exit 23"
failure_output="$( (validate_r_version "$TMP/Rscript") 2>&1 || true)"
for expected in \
  "Rscript: $TMP/Rscript" \
  'Exit status: 23' \
  'stdout diagnostic' \
  'loader diagnostic' \
  "ldd '$TMP/Rscript'"; do
  [[ "$failure_output" == *"$expected"* ]] || {
    printf 'Expected output containing <%s>, got:\n%s\n' "$expected" "$failure_output" >&2
    exit 1
  }
done

make_rscript ':'
assert_failure 'returned an empty R version'

make_rscript "printf 'R version 4.5.2'"
assert_failure "returned malformed R version 'R version 4.5.2'"

make_rscript "printf '4.5.1'"
assert_failure 'Requested R 4.5.2'

echo 'bundle-r version validation checks passed'
