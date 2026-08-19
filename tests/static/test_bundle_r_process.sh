#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "$0")/../.." && pwd)"
TMP="$(mktemp -d)"
trap 'rm -rf "$TMP"' EXIT
FAKE_BIN="$TMP/fake bin"
FAKE_HOME="$TMP/fake R home"
mkdir -p "$FAKE_BIN" "$FAKE_HOME/bin" "$FAKE_HOME/library/base"

cat >"$FAKE_BIN/R" <<'EOF_R'
#!/usr/bin/env bash
[ -z "${R_HOME-}${R_PROFILE_USER-}${R_LIBS_USER-}" ] || { echo contaminated >&2; exit 91; }
printf '%s' "${FAKE_R_OUTPUT-R version 4.5.2 (test)}"
printf '%s' "${FAKE_R_ERROR-}" >&2
exit "${FAKE_R_STATUS:-0}"
EOF_R
cat >"$FAKE_BIN/Rscript" <<'EOF_RSCRIPT'
#!/usr/bin/env bash
[ -z "${R_HOME-}${R_PROFILE_USER-}${R_LIBS_USER-}" ] || { echo contaminated >&2; exit 92; }
if [ "${1-}" = --version ]; then
  printf '%s' "${FAKE_RSCRIPT_OUTPUT-Rscript (R) version 4.5.2}" >&2
  printf '%s' "${FAKE_RSCRIPT_ERROR-}" >&2
  exit "${FAKE_RSCRIPT_STATUS:-0}"
fi
case "${*: -1}" in
  *R.home*) printf '%s' "$FAKE_CONTROLLED_R_HOME" ;;
  *) printf '%s' "${FAKE_EXPRESSION_OUTPUT-4.5.2}" ;;
esac
printf '%s' "${FAKE_EXPRESSION_ERROR-}" >&2
exit "${FAKE_EXPRESSION_STATUS:-0}"
EOF_RSCRIPT
chmod +x "$FAKE_BIN/R" "$FAKE_BIN/Rscript"
cp "$FAKE_BIN/R" "$FAKE_HOME/bin/R"
cp "$FAKE_BIN/Rscript" "$FAKE_HOME/bin/Rscript"
printf 'fixture\n' > "$FAKE_HOME/library/base/DESCRIPTION"

run_case() {
  local name="$1" expected_status="$2" expected="$3"; shift 3
  rm -rf "$TMP/$name"
  set +e
  output="$(env PATH="$FAKE_BIN:$PATH" FAKE_CONTROLLED_R_HOME="$FAKE_HOME" \
    MIRAPROT_TEST_VALIDATE_ONLY=1 "$@" bash "$ROOT/portable/scripts/bundle-r.sh" \
    --r-version 4.5.2 --output-dir "$TMP/$name path with spaces" 2>&1)"
  result=$?
  set -e
  [ "$result" -eq "$expected_status" ] || { printf '%s: expected %s, got %s\n%s\n' "$name" "$expected_status" "$result" "$output" >&2; exit 1; }
  [[ "$output" == *"$expected"* ]] || { printf '%s: missing <%s> in:\n%s\n' "$name" "$expected" "$output" >&2; exit 1; }
}

run_case startup 0 'validated successfully' env
run_case r-mismatch 1 'R --version reports R 4.5.1' env FAKE_R_OUTPUT='R version 4.5.1'
run_case mismatch 1 'Rscript --version reports R 4.5.1' env FAKE_RSCRIPT_OUTPUT='Rscript (R) version 4.5.1'
run_case empty 1 'version expression returned empty output' env FAKE_EXPRESSION_OUTPUT=
run_case nonzero 1 'Captured standard error:' env FAKE_EXPRESSION_STATUS=23 FAKE_EXPRESSION_ERROR='captured loader error'
[[ "$output" == *'captured loader error'* && "$output" == *'Exit status: 23'* ]] || { echo 'stderr/status were not preserved' >&2; exit 1; }
run_case clean-env 0 'Ignoring inherited R environment variables:' env R_HOME=bad R_PROFILE_USER=bad R_LIBS_USER=bad
[[ "$output" != *contaminated* ]] || { echo 'R variables reached child process' >&2; exit 1; }

# Exercise copying from a controlled R home and validation of the exact copied paths.
set +e
output="$(env PATH="$FAKE_BIN:$PATH" FAKE_CONTROLLED_R_HOME="$FAKE_HOME" MIRAPROT_TEST_RUNTIME_ONLY=1 \
  bash "$ROOT/portable/scripts/bundle-r.sh" --r-version 4.5.2 --output-dir "$TMP/copied runtime path" 2>&1)"
result=$?
set -e
[ "$result" -eq 0 ] && [[ "$output" == *'Portable R ready'* && "$output" == *'validated successfully'* ]] || { echo "copied runtime success failed: $output" >&2; exit 1; }

# The source validates, then the copied Rscript independently fails final validation.
rm -rf "$TMP/failing home" "$TMP/copied failure"
cp -a "$FAKE_HOME" "$TMP/failing home"
cat >>"$TMP/failing home/bin/Rscript" <<'EOF_FAIL'
EOF_FAIL
# A marker in the copied home lets the fixture distinguish source PATH from final path.
python3 - "$TMP/failing home/bin/Rscript" <<'PY'
from pathlib import Path
p=Path(__import__('sys').argv[1]); s=p.read_text();
s=s.replace('#!/usr/bin/env bash', '#!/usr/bin/env bash\ncase "$0" in *"copied failure"*) echo copied-runtime-failure >&2; exit 37;; esac', 1)
p.write_text(s)
PY
chmod +x "$TMP/failing home/bin/Rscript"
set +e
output="$(env PATH="$FAKE_BIN:$PATH" FAKE_CONTROLLED_R_HOME="$TMP/failing home" MIRAPROT_TEST_RUNTIME_ONLY=1 \
  bash "$ROOT/portable/scripts/bundle-r.sh" --r-version 4.5.2 --output-dir "$TMP/copied failure" 2>&1)"
result=$?
set -e
[ "$result" -ne 0 ] && [[ "$output" == *copied-runtime-failure* && "$output" == *'Exit status: 37'* ]] || { echo "copied runtime failure was not diagnosed: $output" >&2; exit 1; }

echo 'bundle-r isolated process checks passed'
