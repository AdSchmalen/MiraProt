#!/usr/bin/env bash
# bundle-r.sh — Create a portable MiraProt distribution for Linux or macOS
#
# Usage:
#   ./bundle-r.sh [--r-version 4.6.0] [--output-dir ./dist]
#
# Environment variables (override defaults):
#   R_VERSION   — R version to bundle (default: 4.6.0)
#   OUTPUT_DIR  — Output directory (default: portable/dist)
#
# Prerequisites:
#   - R must be installed (system-wide or via rig)
#   - Go toolchain (for building the launcher)
#   - rsync
#   - On Linux: apt-get access for system library dependencies

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"
R_VERSION="${R_VERSION:-4.6.0}"
OUTPUT_DIR="${OUTPUT_DIR:-$SCRIPT_DIR/../dist}"
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

# -----------------------------------------------------------------------
# Step 1: Obtain portable R
# -----------------------------------------------------------------------
if [ -f "$R_PORTABLE/bin/Rscript" ]; then
  echo "--- R already present at $R_PORTABLE ---"
else
  echo "--- Setting up portable R $R_VERSION ---"

  case "$PLATFORM" in
    linux)
      # On Linux, link the system R installation into r-portable/
      # Install R via: apt install r-base, or rig add <version>
      if command -v Rscript &>/dev/null; then
        R_BIN_DIR="$(dirname "$(command -v Rscript)")"
        R_HOME="$(Rscript -e 'cat(R.home())')"
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
      if command -v Rscript &>/dev/null; then
        R_HOME="$(Rscript -e 'cat(R.home())')"
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
"$R_PORTABLE/bin/Rscript" "$SCRIPT_DIR/install-packages.R" "$R_LIBRARY"
echo ""

# -----------------------------------------------------------------------
# Step 4: Pre-build AnnotationHub cache
# -----------------------------------------------------------------------
GO_CACHE="$OUTPUT_DIR/go-cache"
if [ -d "$GO_CACHE/annotation_cache" ] && [ -d "$GO_CACHE/go_cache" ]; then
  echo "--- go-cache already present at $GO_CACHE ---"
else
  echo "--- Pre-building AnnotationHub cache into $GO_CACHE ---"
  mkdir -p "$GO_CACHE"
  "$R_PORTABLE/bin/Rscript" "$SCRIPT_DIR/prebuild-cache.R" "$GO_CACHE" "$R_LIBRARY"
fi
echo ""

# -----------------------------------------------------------------------
# Step 4b: Seed go-cache from the project's non-portable ./cache/ folder
# -----------------------------------------------------------------------
# The non-portable app persists its annotation caches in:
#   ./cache/GO_Cache/       — GO / AnnotationHub organism caches
#   ./cache/BioMart_Cache/  — BioMart species + mapping caches
#
# The portable launcher exposes these at runtime via:
#   MIRAPROT_GO_CACHE   -> go-cache/go_cache/
#   ANNOTATION_HUB_CACHE -> go-cache/annotation_cache/
#
# BioMart in portable mode stores its cache in $MIRAPROT_GO_CACHE/BioMart_Cache
# (see modules/Data Wizard/Annotation/datawizard_annotation_utils_biomart_cache.R).
#
# Merge the developer's cache into the portable distribution so every cached
# database file is shipped — not just the single organism downloaded by
# prebuild-cache.R. Existing files are overwritten with the project copy so the
# developer's (typically richer) cache wins over the stub from prebuild.
PROJECT_CACHE="$PROJECT_ROOT/cache"
if [ -d "$PROJECT_CACHE" ]; then
  echo "--- Seeding go-cache from $PROJECT_CACHE ---"

  if [ -d "$PROJECT_CACHE/GO_Cache" ]; then
    echo "Copying cache/GO_Cache -> $GO_CACHE/go_cache"
    mkdir -p "$GO_CACHE/go_cache"
    # Trailing slash on source copies contents, preserving organism subdirs.
    # The per-organism <orgdb>.sqlite files land at
    # go-cache/go_cache/<orgdb>/<orgdb>.sqlite, which load_organism_cache()
    # finds via its canonical-path fallback (GO_module_hub.R:1372-1382) even
    # though the sqlite_path stored in cache_metadata.rds was absolute on the
    # developer's machine. Nested ah_cache/ subdirs are preserved but unused
    # at runtime in portable mode (ANNOTATION_HUB_CACHE takes precedence) —
    # we intentionally do NOT merge them into the top-level annotation_cache/
    # because each is its own BiocFileCache with a SQLite index, and merging
    # would clobber the index and leave orphaned blobs.
    rsync -a "$PROJECT_CACHE/GO_Cache/" "$GO_CACHE/go_cache/"
  else
    echo "No cache/GO_Cache/ directory found — skipping GO cache seed."
  fi

  if [ -d "$PROJECT_CACHE/BioMart_Cache" ]; then
    echo "Copying cache/BioMart_Cache -> $GO_CACHE/go_cache/BioMart_Cache"
    mkdir -p "$GO_CACHE/go_cache/BioMart_Cache"
    rsync -a "$PROJECT_CACHE/BioMart_Cache/" "$GO_CACHE/go_cache/BioMart_Cache/"
  else
    echo "No cache/BioMart_Cache/ directory found — skipping BioMart cache seed."
  fi
else
  echo "--- No ./cache/ folder at $PROJECT_CACHE — nothing to seed ---"
fi
echo ""

# -----------------------------------------------------------------------
# Step 5: Copy Shiny application
# -----------------------------------------------------------------------
echo "--- Copying Shiny application ---"
mkdir -p "$SHINY_APP"

RSYNC_EXCLUDES=(
  --exclude='.git'
  --exclude='cache/'
  --exclude='portable/'
  --exclude='.Rproj.user'
  --exclude='.RData'
  --exclude='.Rhistory'
  --exclude='.Ruserdata'
  --exclude='user_data/'
  --exclude='dist/'
)

# If OUTPUT_DIR lives inside PROJECT_ROOT, exclude it explicitly to avoid
# recursively copying previously generated portable bundles into shiny-app/.
case "$OUTPUT_DIR" in
  "$PROJECT_ROOT"/*)
    OUTPUT_DIR_REL="${OUTPUT_DIR#"$PROJECT_ROOT"/}"
    RSYNC_EXCLUDES+=("--exclude=${OUTPUT_DIR_REL%/}/")
    ;;
esac

rsync -a \
  "${RSYNC_EXCLUDES[@]}" \
  "$PROJECT_ROOT/" "$SHINY_APP/"

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
VERSION="$(git -C "$PROJECT_ROOT" describe --tags --always 2>/dev/null || echo "dev")"

(
  cd "$LAUNCHER_DIR"
  go build \
    -ldflags "-s -w -X main.Version=$VERSION" \
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
