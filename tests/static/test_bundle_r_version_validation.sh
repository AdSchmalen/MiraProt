#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "$0")/../.." && pwd)"
SOURCE="$ROOT/portable/scripts/bundle-r.sh"
TMP="$(mktemp -d)"
trap 'rm -rf "$TMP"' EXIT

# Load the helper stack without starting the bundler.
sed -n '/^capture_process() {/,/^# -----------------------------------------------------------------------/p' "$SOURCE" | sed '$d' >"$TMP/functions.sh"
# shellcheck source=/dev/null
source "$TMP/functions.sh"
R_VERSION=4.5.2
PLATFORM=linux

cat >"$TMP/R" <<'EOF'
#!/usr/bin/env bash
printf '%s' "${R_OUTPUT-R version 4.5.2 (test)}"
printf '%s' "${R_ERROR:-}" >&2
exit "${R_STATUS:-0}"
EOF
cat >"$TMP/Rscript" <<'EOF'
#!/usr/bin/env bash
if [ "$1" = --version ]; then
  printf '%s' "${RS_VERSION_OUTPUT-Rscript (R) version 4.5.2}" >&2
  exit "${RS_VERSION_STATUS:-0}"
fi
printf '%s' "${RS_EXPRESSION_OUTPUT-4.5.2}"
printf '%s' "${RS_EXPRESSION_ERROR:-}" >&2
exit "${RS_EXPRESSION_STATUS:-0}"
EOF
chmod +x "$TMP/R" "$TMP/Rscript"

assert_failure() {
  local expected="$1"; shift
  if output="$(env "$@" bash -c 'source "$1"; R_VERSION=4.5.2 PLATFORM=linux; validate_r_installation "$2" "$3"' _ "$TMP/functions.sh" "$TMP/R" "$TMP/Rscript" 2>&1)"; then
    echo "Expected validation to fail." >&2; exit 1
  fi
  [[ "$output" == *"$expected"* ]] || { printf 'Expected <%s>, got:\n%s\n' "$expected" "$output" >&2; exit 1; }
}

validate_r_installation "$TMP/R" "$TMP/Rscript"
assert_failure 'R --version returned empty output' R_OUTPUT=
assert_failure 'R --version returned malformed output' R_OUTPUT='not a version'
assert_failure 'Rscript --version reports R 4.5.1' RS_VERSION_OUTPUT='Rscript (R) version 4.5.1'
assert_failure 'version expression returned malformed output' RS_EXPRESSION_OUTPUT='R version 4.5.2'
assert_failure 'Exit status: 23' RS_EXPRESSION_STATUS=23 RS_EXPRESSION_ERROR='loader diagnostic'
[[ "$output" == *'loader diagnostic'* && "$output" == *'Shared-library diagnostics (ldd):'* ]] || exit 1
missing_output="$(validate_r_installation "$TMP/R" "$TMP/missing-Rscript" 2>&1 || true)"
[[ "$missing_output" == *'Missing executable Rscript'* ]] || exit 1

echo 'bundle-r version validation checks passed'
