#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "$0")/../.." && pwd)"
TMP="$(mktemp -d)"
trap 'rm -rf "$TMP"' EXIT
FAKE="$TMP/bin/Rscript"
mkdir -p "$(dirname "$FAKE")"

run_case() {
  local name="$1" stdout="$2" status="$3" stderr="$4" expected_status="$5" expected="$6"
  cat >"$FAKE" <<EOF
#!/usr/bin/env bash
printf '%s' '$stdout'
printf '%s' '$stderr' >&2
exit $status
EOF
  chmod +x "$FAKE"
  set +e
  output="$(PATH="$TMP/bin:$PATH" MIRAPROT_TEST_VALIDATE_ONLY=1 \
    bash "$ROOT/portable/scripts/bundle-r.sh" --r-version 4.5.2 --output-dir "$TMP/$name" 2>&1)"
  result=$?
  set -e
  [ "$result" -eq "$expected_status" ] || { printf '%s: expected status %s, got %s\n%s\n' "$name" "$expected_status" "$result" "$output" >&2; exit 1; }
  [[ "$output" == *"$expected"* ]] || { printf '%s: missing <%s> in:\n%s\n' "$name" "$expected" "$output" >&2; exit 1; }
  if [ "$expected_status" -ne 0 ]; then
    [ ! -e "$TMP/$name/r-portable" ] || { echo "$name continued with a partial runtime" >&2; exit 1; }
  fi
}

run_case valid 4.5.2 0 '' 0 'validated successfully'
run_case mismatch 4.5.1 0 '' 1 'Requested R 4.5.2'
run_case empty '' 0 '' 1 'returned an empty R version'
run_case nonzero 'partial output' 23 'captured loader error' 1 'Captured standard error:'
[[ "$output" == *'captured loader error'* && "$output" == *'Exit status: 23'* ]] || { echo "nonzero case did not preserve stderr/status" >&2; exit 1; }

echo 'bundle-r isolated process checks passed'
