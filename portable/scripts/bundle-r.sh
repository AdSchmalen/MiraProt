#!/usr/bin/env bash
# bundle-r.sh — Create a portable MiraProt distribution for Linux or macOS
#
# Usage:
#   ./bundle-r.sh [--r-version VERSION] [--output-dir DIRECTORY]
#   Ordinary users should omit --r-version: portable/R_VERSION supplies the
#   maintained R runtime default. This option does not select MiraProt's
#   application version.
#
# Environment variables (fallbacks when the corresponding option is omitted):
#   R_VERSION   — R version to bundle (default: portable/R_VERSION)
#   OUTPUT_DIR  — Output directory (default: portable/dist)
# Command-line options take precedence over these environment variables.
#
# Prerequisites:
#   - R must be installed (system-wide or via rig)
#   - Go toolchain (for building the launcher)
#   - rsync
#   - On Linux: apt-get access for system library dependencies

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"
DEFAULT_R_VERSION="$(tr -d '[:space:]' < "$SCRIPT_DIR/../R_VERSION")"
R_VERSION="${R_VERSION:-$DEFAULT_R_VERSION}"
OUTPUT_DIR="${OUTPUT_DIR:-$SCRIPT_DIR/../dist}"
VERSION_FILE="$PROJECT_ROOT/VERSION"
if [ ! -f "$VERSION_FILE" ]; then
  echo "ERROR: Missing canonical MiraProt VERSION file: $VERSION_FILE" >&2; exit 2
fi
MIRAPROT_VERSION="$(cat "$VERSION_FILE")"
if [ "$(wc -l < "$VERSION_FILE")" -ne 1 ] || [[ ! "$MIRAPROT_VERSION" =~ ^[0-9]+\.[0-9]+\.[0-9]+$ ]]; then
  echo "ERROR: VERSION must contain exactly one MAJOR.MINOR.PATCH value." >&2; exit 2
fi
CFF_VERSION="$(sed -n 's/^version: "\([^"]*\)"$/\1/p' "$PROJECT_ROOT/CITATION.cff")"
if [ "$CFF_VERSION" != "$MIRAPROT_VERSION" ]; then
  echo "ERROR: CITATION.cff version '$CFF_VERSION' does not match VERSION '$MIRAPROT_VERSION'." >&2; exit 2
fi

usage() {
  cat <<EOF
Usage: $0 [--r-version VERSION] [--output-dir DIRECTORY]

Options:
  --r-version VERSION   R runtime version to bundle, not the MiraProt version
                        (default: portable/R_VERSION; normally omit this option)
  --output-dir DIRECTORY
                        Output directory (default: portable/dist)
  -h, --help            Show this help message

R_VERSION and OUTPUT_DIR provide fallbacks. Command-line options take precedence.
MiraProt's application version is independent of the selected R runtime.
EOF
}

usage_error() {
  echo "ERROR: $1" >&2
  usage >&2
  exit 2
}

while [ "$#" -gt 0 ]; do
  case "$1" in
    --r-version)
      [ "$#" -ge 2 ] && [ -n "$2" ] && [[ "$2" != -* ]] || \
        usage_error "--r-version requires a value."
      R_VERSION="$2"
      shift 2
      ;;
    --output-dir)
      [ "$#" -ge 2 ] && [ -n "$2" ] && [[ "$2" != -* ]] || \
        usage_error "--output-dir requires a value."
      OUTPUT_DIR="$2"
      shift 2
      ;;
    -h|--help)
      usage
      exit 0
      ;;
    *)
      usage_error "Unknown argument: $1"
      ;;
  esac
done

if [[ ! "$R_VERSION" =~ ^[0-9]+\.[0-9]+\.[0-9]+$ ]]; then
  echo "ERROR: Invalid R version '$R_VERSION' (expected MAJOR.MINOR.PATCH)." >&2
  exit 2
fi
PLATFORM="$(uname -s | tr '[:upper:]' '[:lower:]')"
ARCH="$(uname -m)"

# Resolve output paths
R_PORTABLE="$OUTPUT_DIR/r-portable"
R_LIBRARY="$OUTPUT_DIR/r-library"
SHINY_APP="$OUTPUT_DIR/shiny-app"

echo "=== MiraProt Portable Bundler ==="
echo "Platform:  $PLATFORM/$ARCH"
echo "R version: $R_VERSION"
echo "Output:    $OUTPUT_DIR"
echo ""

mkdir -p "$OUTPUT_DIR" "$R_LIBRARY"

for documentation_file in LICENSE.md README.md THIRD_PARTY_NOTICES.md CITATION.cff VERSION; do
  if [ -f "$PROJECT_ROOT/$documentation_file" ]; then
    echo "Including portable documentation: $documentation_file"
    cp "$PROJECT_ROOT/$documentation_file" "$OUTPUT_DIR/$documentation_file"
  else
    echo "WARNING: Portable documentation file not found; skipping: $documentation_file" >&2
  fi
done

R_ENVIRONMENT_VARIABLES=(
  R_HOME R_ARCH R_LIBS R_LIBS_USER R_LIBS_SITE
  R_ENVIRON R_ENVIRON_USER R_PROFILE R_PROFILE_USER
)

log_inherited_r_environment() {
  local name contaminated=()
  for name in "${R_ENVIRONMENT_VARIABLES[@]}"; do
    if [[ -v "$name" ]]; then
      contaminated+=("$name")
    fi
  done
  if [ "${#contaminated[@]}" -gt 0 ]; then
    printf 'Ignoring inherited R environment variables: %s\n' \
      "$(IFS=', '; echo "${contaminated[*]}")" >&2
  fi
}

run_with_clean_r_environment() {
  local unset_args=() name
  log_inherited_r_environment
  for name in "${R_ENVIRONMENT_VARIABLES[@]}"; do
    unset_args+=( -u "$name" )
  done
  env "${unset_args[@]}" "$@"
}

capture_process() {
  local capture_dir had_errexit=0
  capture_dir="$(mktemp -d "${TMPDIR:-/tmp}/miraprot-process.XXXXXX")"
  [[ $- == *e* ]] && had_errexit=1
  set +e
  "$@" >"$capture_dir/stdout" 2>"$capture_dir/stderr"
  PROCESS_STATUS=$?
  [ "$had_errexit" -eq 0 ] || set -e
  PROCESS_STDOUT="$(cat "$capture_dir/stdout")"
  PROCESS_STDERR="$(cat "$capture_dir/stderr")"
  rm -rf "$capture_dir"
}

print_process_failure() {
  local label="$1" executable="$2"
  echo "ERROR: $label failed to start or exited unsuccessfully." >&2
  echo "Executable: $executable" >&2
  echo "Exit status: $PROCESS_STATUS" >&2
  echo "Captured standard output:" >&2
  [ -z "$PROCESS_STDOUT" ] && echo "(no standard output)" >&2 || printf '%s\n' "$PROCESS_STDOUT" >&2
  echo "Captured standard error:" >&2
  [ -z "$PROCESS_STDERR" ] && echo "(no standard error)" >&2 || printf '%s\n' "$PROCESS_STDERR" >&2
  case "$PLATFORM" in
    linux)
      echo "Shared-library diagnostics (ldd):" >&2
      ldd "$executable" >&2 2>&1 || true
      echo "Resolve any 'not found' libraries above (Ubuntu/Debian packages), then retry." >&2
      ;;
    darwin)
      echo "Host architecture (uname -m): $(uname -m)" >&2
      echo "Executable architecture metadata:" >&2
      file "$executable" >&2 2>&1 || true
      echo "Ensure R and Rscript match the Intel (x86_64) or Apple Silicon (arm64) host architecture." >&2
      ;;
  esac
}

print_captured_stderr() {
  if [ -n "$PROCESS_STDERR" ]; then
    echo "Captured standard error:" >&2
    printf '%s\n' "$PROCESS_STDERR" >&2
  fi
}

validate_r_installation() {
  local r_command="$1" rscript_command="$2" r_path rscript_path name actual combined
  local probe_env_args=() contaminated=()

  for name in R_HOME R_ARCH R_LIBS R_LIBS_USER R_LIBS_SITE R_ENVIRON R_ENVIRON_USER R_PROFILE R_PROFILE_USER; do
    probe_env_args+=( -u "$name" )
    [[ -v "$name" ]] && contaminated+=("$name")
  done
  [ "${#contaminated[@]}" -eq 0 ] || printf 'Ignoring inherited R environment variables: %s\n' \
    "$(IFS=', '; echo "${contaminated[*]}")" >&2

  r_path="$(command -v -- "$r_command" 2>/dev/null || true)"
  rscript_path="$(command -v -- "$rscript_command" 2>/dev/null || true)"
  if [ -z "$r_path" ] || [ ! -x "$r_path" ]; then
    echo "ERROR: Missing executable R: $r_command" >&2
    return 1
  fi
  if [ -z "$rscript_path" ] || [ ! -x "$rscript_path" ]; then
    echo "ERROR: Missing executable Rscript: $rscript_command" >&2
    return 1
  fi

  capture_process env "${probe_env_args[@]}" "$r_path" --version
  if [ "$PROCESS_STATUS" -ne 0 ]; then print_process_failure "R --version" "$r_path"; return 1; fi
  combined="$(printf '%s\n%s' "$PROCESS_STDOUT" "$PROCESS_STDERR" | sed -e 's/^[[:space:]]*//' -e 's/[[:space:]]*$//')"
  if [ -z "$combined" ]; then echo "ERROR: R --version returned empty output: $r_path" >&2; print_captured_stderr; return 1; fi
  if [[ ! "$combined" =~ R[[:space:]]+version[[:space:]]+([0-9]+\.[0-9]+\.[0-9]+) ]]; then
    echo "ERROR: R --version returned malformed output: $combined" >&2; print_captured_stderr; return 1
  fi
  actual="${BASH_REMATCH[1]}"
  if [ "$actual" != "$R_VERSION" ]; then echo "ERROR: Requested R $R_VERSION, but R --version reports R $actual." >&2; print_captured_stderr; return 1; fi

  capture_process env "${probe_env_args[@]}" "$rscript_path" --version
  if [ "$PROCESS_STATUS" -ne 0 ]; then print_process_failure "Rscript --version" "$rscript_path"; return 1; fi
  combined="$(printf '%s\n%s' "$PROCESS_STDOUT" "$PROCESS_STDERR" | sed -e 's/^[[:space:]]*//' -e 's/[[:space:]]*$//')"
  if [ -z "$combined" ]; then echo "ERROR: Rscript --version returned empty output: $rscript_path" >&2; print_captured_stderr; return 1; fi
  if [[ ! "$combined" =~ version[[:space:]]+([0-9]+\.[0-9]+\.[0-9]+) ]]; then
    echo "ERROR: Rscript --version returned malformed output: $combined" >&2; print_captured_stderr; return 1
  fi
  actual="${BASH_REMATCH[1]}"
  if [ "$actual" != "$R_VERSION" ]; then echo "ERROR: Requested R $R_VERSION, but Rscript --version reports R $actual." >&2; print_captured_stderr; return 1; fi

  capture_process env "${probe_env_args[@]}" "$rscript_path" --vanilla -s -e 'cat(as.character(getRversion()))'
  if [ "$PROCESS_STATUS" -ne 0 ]; then print_process_failure "R version expression" "$rscript_path"; return 1; fi
  actual="$(printf '%s' "$PROCESS_STDOUT" | sed -e 's/^[[:space:]]*//' -e 's/[[:space:]]*$//')"
  if [ -z "$actual" ]; then echo "ERROR: R version expression returned empty output: $rscript_path" >&2; print_captured_stderr; return 1; fi
  if [[ ! "$actual" =~ ^[0-9]+\.[0-9]+\.[0-9]+$ ]]; then echo "ERROR: R version expression returned malformed output: '$actual'." >&2; print_captured_stderr; return 1; fi
  if [ "$actual" != "$R_VERSION" ]; then
    echo "ERROR: Requested R $R_VERSION, but the R version expression reports R $actual." >&2
    print_captured_stderr
    echo "Install/select R $R_VERSION (for example: rig add $R_VERSION && rig default $R_VERSION), then retry." >&2
    return 1
  fi
  echo "R version $actual validated successfully."
}

# -----------------------------------------------------------------------
# Step 1: Obtain portable R
# -----------------------------------------------------------------------
if [ -f "$R_PORTABLE/bin/Rscript" ]; then
  validate_r_installation "$R_PORTABLE/bin/R" "$R_PORTABLE/bin/Rscript"
  if [ "${MIRAPROT_TEST_VALIDATE_ONLY:-0}" = 1 ]; then exit 0; fi
  echo "--- R already present at $R_PORTABLE ---"
else
  echo "--- Setting up portable R $R_VERSION ---"

  case "$PLATFORM" in
    linux)
      # On Linux, link the system R installation into r-portable/
      # Install R via: apt install r-base, or rig add <version>
      if command -v R &>/dev/null && command -v Rscript &>/dev/null; then
        validate_r_installation "$(command -v R)" "$(command -v Rscript)"
        if [ "${MIRAPROT_TEST_VALIDATE_ONLY:-0}" = 1 ]; then exit 0; fi
        R_BIN_DIR="$(dirname "$(command -v Rscript)")"
        R_HOME="$(run_with_clean_r_environment Rscript --vanilla -s -e 'cat(R.home())')"
        echo "Found system R at: $R_HOME"
        # Copy the entire R installation for true portability
        echo "Copying R installation to $R_PORTABLE..."
        cp -a "$R_HOME" "$R_PORTABLE"
        # Ensure bin/ scripts exist at the expected path
        if [ ! -f "$R_PORTABLE/bin/Rscript" ]; then
          mkdir -p "$R_PORTABLE/bin"
          cp "$R_BIN_DIR/R" "$R_PORTABLE/bin/" 2>/dev/null || true
          cp "$R_BIN_DIR/Rscript" "$R_PORTABLE/bin/" 2>/dev/null || true
        fi
      else
        echo "ERROR: R not found on this system."
        echo "Install R $R_VERSION first:"
        echo "  Ubuntu/Debian: sudo apt install r-base"
        echo "  Via rig:       rig add $R_VERSION"
        exit 1
      fi
      ;;

    darwin)
      # On macOS, use the system/Homebrew/rig R installation
      if [[ "$ARCH" != x86_64 && "$ARCH" != arm64 ]]; then
        echo "ERROR: Unsupported macOS architecture: $ARCH (expected x86_64 or arm64)." >&2
        exit 1
      fi
      if command -v R &>/dev/null && command -v Rscript &>/dev/null; then
        validate_r_installation "$(command -v R)" "$(command -v Rscript)"
        if [ "${MIRAPROT_TEST_VALIDATE_ONLY:-0}" = 1 ]; then exit 0; fi
        R_HOME="$(run_with_clean_r_environment Rscript --vanilla -s -e 'cat(R.home())')"
        echo "Found system R at: $R_HOME"
        echo "Copying R installation to $R_PORTABLE..."
        cp -a "$R_HOME" "$R_PORTABLE"
        if [ ! -f "$R_PORTABLE/bin/Rscript" ]; then
          mkdir -p "$R_PORTABLE/bin"
          R_BIN_DIR="$(dirname "$(command -v Rscript)")"
          cp "$R_BIN_DIR/R" "$R_PORTABLE/bin/" 2>/dev/null || true
          cp "$R_BIN_DIR/Rscript" "$R_PORTABLE/bin/" 2>/dev/null || true
        fi
      else
        echo "ERROR: R not found on this system."
        echo "Install R $R_VERSION first:"
        echo "  Homebrew: brew install r"
        echo "  Via rig:  rig add $R_VERSION"
        exit 1
      fi
      ;;

    *)
      echo "ERROR: Unsupported platform: $PLATFORM"
      echo "Use bundle-r-windows.ps1 on Windows."
      exit 1
      ;;
  esac

  echo "Portable R ready at: $R_PORTABLE"
fi

# Wrapper repair above can change which binary is reached. Validate the exact
# final paths before any package installation or other R execution.
validate_r_installation "$R_PORTABLE/bin/R" "$R_PORTABLE/bin/Rscript"
if [ "${MIRAPROT_TEST_RUNTIME_ONLY:-0}" = 1 ]; then exit 0; fi
echo ""

# -----------------------------------------------------------------------
# Step 2: Install system dependencies (Linux only)
# -----------------------------------------------------------------------
if [ "$PLATFORM" = "linux" ]; then
  echo "--- Checking system dependencies ---"
  REQUIRED_LIBS=(
    libfreetype6-dev
    libfontconfig1-dev
    libharfbuzz-dev
    libfribidi-dev
    libtiff5-dev
    libjpeg-dev
    libpng-dev
    librsvg2-dev
  )

  MISSING_LIBS=()
  for lib in "${REQUIRED_LIBS[@]}"; do
    if ! dpkg -l "$lib" &>/dev/null 2>&1; then
      MISSING_LIBS+=("$lib")
    fi
  done

  if [ ${#MISSING_LIBS[@]} -gt 0 ]; then
    echo "Installing missing system libraries: ${MISSING_LIBS[*]}"
    sudo apt-get update -qq
    sudo apt-get install -y --no-install-recommends "${MISSING_LIBS[@]}"
  else
    echo "All system dependencies present."
  fi
  echo ""
fi

# -----------------------------------------------------------------------
# Step 3: Install R packages
# -----------------------------------------------------------------------
echo "--- Installing R packages into $R_LIBRARY ---"
run_with_clean_r_environment R_LIBS_USER="$R_LIBRARY" \
  "$R_PORTABLE/bin/Rscript" --vanilla "$SCRIPT_DIR/install-packages.R" "$R_LIBRARY"
echo ""

# -----------------------------------------------------------------------
# Step 4: Seed portable caches, then fill missing AnnotationHub/GO pieces
# -----------------------------------------------------------------------
GO_CACHE="$OUTPUT_DIR/go-cache"
PROJECT_CACHE="$PROJECT_ROOT/cache"
mkdir -p "$GO_CACHE"

echo "--- Seeding portable caches from existing MiraProt cache ---"
if [ -d "$PROJECT_CACHE/GO_Cache" ]; then
  echo "Reusing source GO cache: cache/GO_Cache"
  mkdir -p "$GO_CACHE/go_cache"
  if ! rsync -a "$PROJECT_CACHE/GO_Cache/" "$GO_CACHE/go_cache/"; then
    echo "WARNING: Could not copy source GO cache; portable prebuild will fill required cache if possible." >&2
  fi
else
  echo "No source GO cache found - portable prebuild will fill required cache if possible."
fi

if [ -d "$PROJECT_CACHE/BioMart_Cache" ]; then
  echo "Reusing source BioMart cache: cache/BioMart_Cache"
  mkdir -p "$GO_CACHE/go_cache/BioMart_Cache"
  if ! rsync -a "$PROJECT_CACHE/BioMart_Cache/" "$GO_CACHE/go_cache/BioMart_Cache/"; then
    echo "WARNING: Could not copy source BioMart cache; BioMart will remain available on demand." >&2
  fi
else
  echo "No source BioMart cache found - BioMart will populate its cache on demand."
fi

# A per-organism ah_cache is an independent BiocFileCache. Seed only the
# canonical human cache, as a complete unit, and never merge organism caches.
SOURCE_AH="$PROJECT_CACHE/GO_Cache/org.Hs.eg.db/ah_cache"
TARGET_AH="$GO_CACHE/annotation_cache"
if [ -d "$SOURCE_AH" ] && { [ -f "$SOURCE_AH/annotationhub.sqlite3" ] || [ -f "$SOURCE_AH/annotationhub.index.rds" ]; }; then
  if [ ! -d "$TARGET_AH" ] || [ -z "$(find "$TARGET_AH" -mindepth 1 -print -quit 2>/dev/null)" ]; then
    echo "Reusing complete source AnnotationHub cache: cache/GO_Cache/org.Hs.eg.db/ah_cache"
    rm -rf "$TARGET_AH"
    if ! rsync -a "$SOURCE_AH/" "$TARGET_AH/"; then
      echo "WARNING: Could not copy source AnnotationHub cache; portable prebuild will use its fallback." >&2
      rm -rf "$TARGET_AH"
    fi
  fi
fi

echo "--- Validating and filling portable AnnotationHub/GO caches ---"
if ! run_with_clean_r_environment R_LIBS_USER="$R_LIBRARY" \
  "$R_PORTABLE/bin/Rscript" --vanilla "$SCRIPT_DIR/prebuild-cache.R" "$GO_CACHE" "$R_LIBRARY"; then
  echo "WARNING: Cache pre-build failed - portable app will download on first use" >&2
fi
echo ""

# -----------------------------------------------------------------------
# Step 5: Copy Shiny application
# -----------------------------------------------------------------------
echo "--- Copying Shiny application ---"
rm -rf "$SHINY_APP"
mkdir -p "$SHINY_APP"

# Runtime payload manifest.  Keep this allowlist synchronized with the Windows
# bundler and portable-build.yml; BUILD_INFO is generated immediately below.
for runtime_dir in R modules AutoAssign GSEA; do
  if [ "$runtime_dir" = GSEA ]; then
    rsync -a --exclude='*.gmt' "$PROJECT_ROOT/$runtime_dir/" "$SHINY_APP/$runtime_dir/"
  else
    rsync -a "$PROJECT_ROOT/$runtime_dir/" "$SHINY_APP/$runtime_dir/"
  fi
done
had_nullglob=false
shopt -q nullglob && had_nullglob=true
shopt -s nullglob
local_gmt_files=("$PROJECT_ROOT"/GSEA/*.gmt)
if [ "$had_nullglob" = false ]; then
  shopt -u nullglob
fi
if [ "${#local_gmt_files[@]}" -gt 0 ]; then
  cp -- "${local_gmt_files[@]}" "$SHINY_APP/GSEA/"
  echo "Included ${#local_gmt_files[@]} local GSEA GMT file(s)."
else
  echo "No local GSEA GMT files found; continuing without bundled gene sets."
fi
if [ ! -f "$PROJECT_ROOT/GSEA/README.md" ]; then
  echo "WARNING: GSEA/README.md not found; portable build will continue without it." >&2
fi
mkdir -p "$SHINY_APP/Documentation"
rsync -a --include='*.R' --exclude='*' \
  "$PROJECT_ROOT/Documentation/" "$SHINY_APP/Documentation/"
for runtime_file in app.R MiraProt_icon.png; do
  cp "$PROJECT_ROOT/$runtime_file" "$SHINY_APP/$runtime_file"
done
cp "$VERSION_FILE" "$SHINY_APP/VERSION"

# Source-development renv state must never enter the standalone application.
# The allowlist above already omits these paths; remove them defensively if the
# runtime payload manifest is expanded in the future.
rm -rf "$SHINY_APP/renv" "$SHINY_APP/.Rprofile" "$SHINY_APP/renv.lock"

echo "App copied to: $SHINY_APP"
{
  echo "COMMIT_COUNT=$(git -C "$PROJECT_ROOT" rev-list --count HEAD)"
  echo "COMMIT_SHA=$(git -C "$PROJECT_ROOT" rev-parse --short=7 HEAD)"
  echo "COMMIT_DATE=$(git -C "$PROJECT_ROOT" log -1 --format=%cs)"
} > "$SHINY_APP/BUILD_INFO"
echo ""

# -----------------------------------------------------------------------
# Step 6: Build Go launcher
# -----------------------------------------------------------------------
echo "--- Building Go launcher ---"
LAUNCHER_DIR="$SCRIPT_DIR/../launcher"
(
  cd "$LAUNCHER_DIR"
  go build \
    -ldflags "-s -w -X main.Version=$MIRAPROT_VERSION" \
    -o "$OUTPUT_DIR/MiraProt-launcher" .
)

echo "Launcher built: $OUTPUT_DIR/MiraProt-launcher"
echo ""

# -----------------------------------------------------------------------
# Done
# -----------------------------------------------------------------------
echo "=== Bundle complete ==="
echo ""
echo "Contents of $OUTPUT_DIR:"
ls -lh "$OUTPUT_DIR/"
echo ""
echo "Directory sizes:"
du -sh "$OUTPUT_DIR"/* 2>/dev/null || true
echo ""
echo "To run: $OUTPUT_DIR/MiraProt-launcher"
