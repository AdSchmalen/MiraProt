#!/usr/bin/env bash
# Exercise bundle-r.sh's argument parser without starting the expensive bundle.
set -euo pipefail

ROOT="$(cd "$(dirname "$0")/../.." && pwd)"
SOURCE="$ROOT/portable/scripts/bundle-r.sh"
HARNESS="$(mktemp "$ROOT/portable/scripts/.bundle-r-cli-test.XXXXXX")"
trap 'rm -f "$HARNESS"' EXIT

# Keep the real initialization/parser and replace the bundling phase with output.
sed '/^PLATFORM=/,$d' "$SOURCE" > "$HARNESS"
cat >> "$HARNESS" <<'EOF'
printf 'R_VERSION=<%s>\nOUTPUT_DIR=<%s>\n' "$R_VERSION" "$OUTPUT_DIR"
EOF
chmod +x "$HARNESS"

DEFAULT_VERSION="$(tr -d '[:space:]' < "$ROOT/portable/R_VERSION")"
DEFAULT_OUTPUT="$ROOT/portable/scripts/../dist"

assert_output() {
  local expected="$1"
  shift
  local actual
  actual="$("$@")"
  [ "$actual" = "$expected" ] || {
    printf 'Expected:\n%s\nActual:\n%s\n' "$expected" "$actual" >&2
    exit 1
  }
}

assert_output "R_VERSION=<$DEFAULT_VERSION>
OUTPUT_DIR=<$DEFAULT_OUTPUT>" env -u R_VERSION -u OUTPUT_DIR "$HARNESS"
assert_output 'R_VERSION=<4.4.3>
OUTPUT_DIR=</tmp/MiraProt output>' "$HARNESS" --r-version 4.4.3 --output-dir '/tmp/MiraProt output'
assert_output 'R_VERSION=<4.3.2>
OUTPUT_DIR=</tmp/environment output>' env R_VERSION=4.3.2 OUTPUT_DIR='/tmp/environment output' "$HARNESS"
assert_output 'R_VERSION=<4.4.1>
OUTPUT_DIR=</tmp/CLI output>' env R_VERSION=4.3.2 OUTPUT_DIR='/tmp/environment output' \
  "$HARNESS" --r-version 4.4.1 --output-dir '/tmp/CLI output'

for args in '--unknown' '--r-version' '--r-version --output-dir value' '--output-dir'; do
  # Deliberate word splitting supplies each test case as separate arguments.
  if "$HARNESS" $args > /dev/null 2>"$HARNESS.err"; then
    echo "Expected failure for: $args" >&2
    exit 1
  fi
  rg -q '^Usage:' "$HARNESS.err"
done
rm -f "$HARNESS.err"

echo 'bundle-r CLI checks passed'
