#!/usr/bin/env bash
# Stage-0 preflight and logging wrapper. Stage 1 remains bundle-r.sh.
set -u
set -o pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"
INTERACTIVE=0
OUTPUT_DIR="$PROJECT_ROOT/portable/dist"
R_VERSION="$(tr -d '[:space:]' < "$PROJECT_ROOT/portable/R_VERSION")"

usage() {
  cat <<EOF
Usage: $0 [--interactive] [--output-dir DIRECTORY] [--r-version VERSION]
EOF
}
die_usage() { echo "ERROR: $1" >&2; usage >&2; exit 2; }
while [ "$#" -gt 0 ]; do
  case "$1" in
    --interactive) INTERACTIVE=1; shift ;;
    --output-dir) [ "$#" -ge 2 ] && [ -n "$2" ] || die_usage "--output-dir requires a value."; OUTPUT_DIR="$2"; shift 2 ;;
    --r-version) [ "$#" -ge 2 ] && [ -n "$2" ] || die_usage "--r-version requires a value."; R_VERSION="$2"; shift 2 ;;
    -h|--help) usage; exit 0 ;;
    *) die_usage "Unknown argument: $1" ;;
  esac
done

case "$OUTPUT_DIR" in /*) ;; *) OUTPUT_DIR="$PROJECT_ROOT/$OUTPUT_DIR" ;; esac
PLATFORM="$(uname -s)"
case "$PLATFORM" in
  Darwin) PLATFORM_NAME=macos ;;
  Linux) PLATFORM_NAME=linux ;;
  *) echo "ERROR: Unsupported operating system: $PLATFORM" >&2; exit 1 ;;
esac
mkdir -p "$PROJECT_ROOT/portable/logs" || exit 1
LOG_FILE="$PROJECT_ROOT/portable/logs/build-$(date '+%Y-%m-%d-%H%M%S')-$PLATFORM_NAME.log"
: > "$LOG_FILE" || { echo "ERROR: Cannot write log: $LOG_FILE" >&2; exit 1; }

log() { printf '[%s] %s\n' "$(date '+%Y-%m-%d %H:%M:%S')" "$*" | tee -a "$LOG_FILE"; }
fail() { log "PREFLIGHT FAIL: $*"; finish 1; }
finish() {
  local status="$1"
  if [ "$status" -eq 0 ]; then log "FINAL STATUS: SUCCESS"; else log "FINAL STATUS: FAILED"; fi
  log "EXIT CODE: $status"
  if [ "$INTERACTIVE" -eq 1 ]; then
    printf 'Press Enter to close...' | tee -a "$LOG_FILE"
    IFS= read -r _ || true
  fi
  exit "$status"
}
pass() { log "PREFLIGHT PASS: $*"; }
version_ge() { [ "$(printf '%s\n%s\n' "$2" "$1" | sort -V | head -n1)" = "$2" ]; }

log "MiraProt Stage-0 build wrapper started"
log "Platform: $PLATFORM ($(uname -m)); repository: $PROJECT_ROOT"
log "WARNING: Portable $PLATFORM_NAME packaging is experimental and requires native end-to-end validation."
case "$(uname -m)" in x86_64|amd64|arm64|aarch64) pass "supported 64-bit architecture $(uname -m)" ;; *) fail "unsupported architecture $(uname -m); a 64-bit x86 or ARM host is required" ;; esac
[[ "$R_VERSION" =~ ^[0-9]+\.[0-9]+\.[0-9]+$ ]] || fail "portable/R_VERSION or --r-version must be MAJOR.MINOR.PATCH (got '$R_VERSION')"
[ -f "$PROJECT_ROOT/VERSION" ] || fail "missing VERSION"
APP_VERSION="$(cat "$PROJECT_ROOT/VERSION")"
[[ "$APP_VERSION" =~ ^[0-9]+\.[0-9]+\.[0-9]+$ ]] || fail "VERSION must contain one MAJOR.MINOR.PATCH value"
[ "$(wc -l < "$PROJECT_ROOT/VERSION")" -eq 1 ] || fail "VERSION must contain exactly one line"
pass "VERSION is $APP_VERSION"
[ -f "$PROJECT_ROOT/renv.lock" ] || fail "missing renv.lock"
LOCK_R_VERSION="$(awk '/^[[:space:]]*"R"[[:space:]]*:/ { inr=1; next } inr && /"Version"[[:space:]]*:/ { line=$0; sub(/^.*"Version"[[:space:]]*:[[:space:]]*"/,"",line); sub(/".*$/,"",line); print line; exit }' "$PROJECT_ROOT/renv.lock")"
[ "$LOCK_R_VERSION" = "$R_VERSION" ] || fail "renv.lock top-level R version '$LOCK_R_VERSION' does not match requested R '$R_VERSION'"
pass "renv.lock top-level R version is $LOCK_R_VERSION"
for command in R Rscript go rsync; do command -v "$command" >/dev/null 2>&1 || fail "$command was not found on PATH"; done
if [ -d "$PROJECT_ROOT/.git" ]; then command -v git >/dev/null 2>&1 || fail "Git is required for this Git checkout"; pass "Git is available for checkout metadata"; else log "PREFLIGHT SKIP: Git is not required for a source archive"; fi
GO_VERSION="$(go version 2>/dev/null | sed -n 's/.* go\([0-9][0-9.]*\).*/\1/p')"
[ -n "$GO_VERSION" ] && version_ge "$GO_VERSION" "1.22" || fail "Go 1.22 or later is required (detected '${GO_VERSION:-unknown}')"
pass "Go $GO_VERSION is available"
ACTUAL_R="$(Rscript --vanilla -s -e 'cat(as.character(getRversion()))' 2>>"$LOG_FILE")" || fail "Rscript authoritative getRversion() probe failed"
[ "$ACTUAL_R" = "$R_VERSION" ] || fail "native R reports $ACTUAL_R; requested $R_VERSION"
R --version >/dev/null 2>>"$LOG_FILE" || fail "native R failed to start"
pass "native R and Rscript report authoritative R $ACTUAL_R"
if [ "$PLATFORM" = Darwin ]; then xcode-select -p >/dev/null 2>&1 || fail "Xcode Command Line Tools are required"; pass "Xcode Command Line Tools are installed"; fi
if [ "$PLATFORM" = Linux ]; then
  command -v apt-get >/dev/null 2>&1 || fail "apt-get is required for the experimental Linux builder"
  command -v dpkg >/dev/null 2>&1 || fail "dpkg is required for the experimental Linux builder"
  pass "apt-get and dpkg are available"
fi
if command -v curl >/dev/null 2>&1; then curl -fsSI --max-time 15 https://cloud.r-project.org/ >/dev/null 2>>"$LOG_FILE" || fail "cannot reach CRAN"; elif command -v wget >/dev/null 2>&1; then wget -q --spider --timeout=15 https://cloud.r-project.org/ 2>>"$LOG_FILE" || fail "cannot reach CRAN"; else fail "curl or wget is required for the CRAN connectivity check"; fi
pass "CRAN is reachable"
mkdir -p "$OUTPUT_DIR" || fail "cannot create output directory $OUTPUT_DIR"
WRITE_PROBE="$OUTPUT_DIR/.miraprot-write-test-$$"
printf test > "$WRITE_PROBE" 2>>"$LOG_FILE" || fail "output directory is not writable: $OUTPUT_DIR"
rm -f "$WRITE_PROBE"
pass "output directory is writable and will be retained: $OUTPUT_DIR"

BUILDER=("$SCRIPT_DIR/bundle-r.sh" --r-version "$R_VERSION" --output-dir "$OUTPUT_DIR")
printf -v RENDERED '%q ' "${BUILDER[@]}"
log "BUILDER INVOCATION: ${RENDERED% }"
"${BUILDER[@]}" 2>&1 | tee -a "$LOG_FILE"
BUILDER_STATUS=${PIPESTATUS[0]}
log "BUILDER EXIT CODE: $BUILDER_STATUS"
[ "$BUILDER_STATUS" -eq 0 ] || finish "$BUILDER_STATUS"
LAUNCHER="$OUTPUT_DIR/MiraProt-launcher"
[ -s "$LAUNCHER" ] || fail "builder succeeded but launcher is missing or empty: $LAUNCHER"
VERSION_CAPTURE="${TMPDIR:-/tmp}/miraprot-version-$$"
"$LAUNCHER" --version 2>&1 | tee -a "$LOG_FILE" "$VERSION_CAPTURE"
VERIFY_STATUS=${PIPESTATUS[0]}
VERSION_OUTPUT="$(cat "$VERSION_CAPTURE")"
rm -f "$VERSION_CAPTURE"
[ "$VERIFY_STATUS" -eq 0 ] || fail "launcher --version failed with exit code $VERIFY_STATUS"
[ -n "$VERSION_OUTPUT" ] || fail "launcher --version returned empty output"
log "VERIFICATION PASS: non-empty launcher and successful --version invocation"
finish 0
