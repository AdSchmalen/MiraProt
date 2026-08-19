#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "$0")/../.." && pwd)"
TMP="$(mktemp -d)"
trap 'rm -rf "$TMP"' EXIT
mkdir -p "$TMP/bin"

cat >"$TMP/bin/R" <<'EOF'
#!/usr/bin/env bash
printf 'R version %s (test)\n' "${FAKE_R_VERSION:-4.5.2}"
EOF
cat >"$TMP/bin/Rscript" <<'EOF'
#!/usr/bin/env bash
if [ "$1" = --version ]; then
  printf 'Rscript (R) version %s\n' "${FAKE_RSCRIPT_VERSION:-4.5.2}" >&2
else
  printf '%s' "${FAKE_EXPRESSION_OUTPUT-4.5.2}"
  printf '%s' "${FAKE_EXPRESSION_ERROR:-}" >&2
  exit "${FAKE_EXPRESSION_STATUS:-0}"
fi
EOF
chmod +x "$TMP/bin/R" "$TMP/bin/Rscript"

run_case() {
  local name="$1" expected_status="$2" expected="$3"
  shift 3
  set +e
  output="$(env PATH="$TMP/bin:$PATH" MIRAPROT_TEST_VALIDATE_ONLY=1 "$@" \
    bash "$ROOT/portable/scripts/bundle-r.sh" --r-version 4.5.2 --output-dir "$TMP/$name" 2>&1)"
  result=$?
  set -e
  [ "$result" -eq "$expected_status" ] || { printf '%s: expected status %s, got %s\n%s\n' "$name" "$expected_status" "$result" "$output" >&2; exit 1; }
  [[ "$output" == *"$expected"* ]] || { printf '%s: missing <%s> in:\n%s\n' "$name" "$expected" "$output" >&2; exit 1; }
}

run_case valid 0 'validated successfully' env
run_case mismatch 1 'Rscript --version reports R 4.5.1' env FAKE_RSCRIPT_VERSION=4.5.1
run_case empty 1 'version expression returned empty output' env FAKE_EXPRESSION_OUTPUT=
run_case nonzero 1 'Captured standard error:' env FAKE_EXPRESSION_STATUS=23 FAKE_EXPRESSION_ERROR='captured loader error'
[[ "$output" == *'captured loader error'* && "$output" == *'Exit status: 23'* && "$output" == *'Shared-library diagnostics (ldd):'* ]] || {
  echo "nonzero case did not preserve stderr/status/ldd diagnostics" >&2; exit 1;
}

echo 'bundle-r isolated process checks passed'
