#!/usr/bin/env bash
# =============================================================================
# create-appimage.sh — Package MiraProt as a Linux AppImage
# =============================================================================
# Usage:
#   ./create-appimage.sh --dist-dir ./dist --version 1.0.0 [--output-dir ./output]
#
# Expects dist-dir to contain:
#   MiraProt-launcher   (Go binary)
#   shiny-app/           (Shiny application)
#   r-portable/          (R installation)
#   r-library/           (R packages)
#
# Downloads appimagetool automatically if not found in PATH.
# =============================================================================

set -euo pipefail

DIST_DIR=""
VERSION="dev"
OUTPUT_DIR=""
APP_NAME="MiraProt"
ARCH="$(uname -m)"

# Parse arguments
while [[ $# -gt 0 ]]; do
  case "$1" in
    --dist-dir)   DIST_DIR="$2"; shift 2 ;;
    --version)    VERSION="$2"; shift 2 ;;
    --output-dir) OUTPUT_DIR="$2"; shift 2 ;;
    *) echo "Unknown argument: $1"; exit 1 ;;
  esac
done

if [ -z "$DIST_DIR" ]; then
  echo "ERROR: --dist-dir is required"
  echo "Usage: $0 --dist-dir ./dist --version 1.0.0"
  exit 1
fi

DIST_DIR="$(cd "$DIST_DIR" && pwd)"
OUTPUT_DIR="${OUTPUT_DIR:-$(pwd)}"
mkdir -p "$OUTPUT_DIR"

APPDIR="$OUTPUT_DIR/${APP_NAME}.AppDir"
APPIMAGE_NAME="${APP_NAME}-${VERSION}-linux-${ARCH}.AppImage"

echo "=== Creating Linux AppImage ==="
echo "Version: $VERSION"
echo "Arch:    $ARCH"
echo "Source:  $DIST_DIR"
echo "Output:  $OUTPUT_DIR"
echo ""

# -----------------------------------------------------------------------
# Step 1: Get appimagetool
# -----------------------------------------------------------------------
if command -v appimagetool &>/dev/null; then
  APPIMAGETOOL="appimagetool"
else
  echo "--- Downloading appimagetool ---"
  APPIMAGETOOL="$OUTPUT_DIR/appimagetool"
  if [ ! -f "$APPIMAGETOOL" ]; then
    TOOL_ARCH="$ARCH"
    # appimagetool uses x86_64 naming
    if [ "$TOOL_ARCH" = "amd64" ]; then
      TOOL_ARCH="x86_64"
    fi
    curl -fsSL -o "$APPIMAGETOOL" \
      "https://github.com/AppImage/appimagetool/releases/download/continuous/appimagetool-${TOOL_ARCH}.AppImage"
    chmod +x "$APPIMAGETOOL"
  fi
  echo "appimagetool ready: $APPIMAGETOOL"
  echo ""
fi

# -----------------------------------------------------------------------
# Step 2: Create AppDir structure
# -----------------------------------------------------------------------
echo "--- Building AppDir ---"

rm -rf "$APPDIR"
mkdir -p "$APPDIR/usr/bin"
mkdir -p "$APPDIR/usr/share/icons/hicolor/256x256/apps"

# Copy launcher binary
cp "$DIST_DIR/MiraProt-launcher" "$APPDIR/usr/bin/MiraProt-launcher"
chmod +x "$APPDIR/usr/bin/MiraProt-launcher"

# Copy application data alongside the binary (launcher resolves paths relative to itself)
cp -a "$DIST_DIR/shiny-app" "$APPDIR/usr/bin/shiny-app"
cp -a "$DIST_DIR/r-portable" "$APPDIR/usr/bin/r-portable"
cp -a "$DIST_DIR/r-library" "$APPDIR/usr/bin/r-library"

# Keep pre-built caches outside usr/bin so the launcher recognizes them as a
# read-only packaged seed rather than the flat bundle's writable adjacent cache.
cp -a "$DIST_DIR/go-cache" "$APPDIR/usr/go-cache"

# -----------------------------------------------------------------------
# Step 3: Create AppRun entry point
# -----------------------------------------------------------------------
cat > "$APPDIR/AppRun" << 'APPRUN_EOF'
#!/usr/bin/env bash
# AppRun — entry point for the MiraProt AppImage
HERE="$(dirname "$(readlink -f "$0")")"
exec "$HERE/usr/bin/MiraProt-launcher" "$@"
APPRUN_EOF
chmod +x "$APPDIR/AppRun"

# -----------------------------------------------------------------------
# Step 4: Create .desktop file
# -----------------------------------------------------------------------
cat > "$APPDIR/MiraProt.desktop" << DESKTOP_EOF
[Desktop Entry]
Type=Application
Name=MiraProt
Comment=Interactive proteomics data analysis
Exec=MiraProt-launcher
Icon=miraprot
Categories=Science;Education;
Terminal=false
DESKTOP_EOF

# -----------------------------------------------------------------------
# Step 5: Create a simple icon (PNG)
# -----------------------------------------------------------------------
# Use existing icon if available, otherwise create a minimal placeholder
if [ -f "$DIST_DIR/MiraProt.png" ]; then
  cp "$DIST_DIR/MiraProt.png" "$APPDIR/MiraProt.png"
elif [ -f "$(dirname "$0")/../../resources/MiraProt.png" ]; then
  cp "$(dirname "$0")/../../resources/MiraProt.png" "$APPDIR/MiraProt.png"
else
  # Create a 1x1 transparent PNG as a minimal placeholder
  printf '\x89PNG\r\n\x1a\n\x00\x00\x00\rIHDR\x00\x00\x00\x01\x00\x00\x00\x01\x08\x06\x00\x00\x00\x1f\x15\xc4\x89\x00\x00\x00\nIDATx\x9cc\x00\x01\x00\x00\x05\x00\x01\r\n\xb4\x00\x00\x00\x00IEND\xaeB`\x82' \
    > "$APPDIR/MiraProt.png"
fi

# Copy icon to hicolor theme directory too
cp "$APPDIR/MiraProt.png" "$APPDIR/usr/share/icons/hicolor/256x256/apps/MiraProt.png"

echo "AppDir created: $APPDIR"
echo ""

# -----------------------------------------------------------------------
# Step 6: Build AppImage
# -----------------------------------------------------------------------
echo "--- Building AppImage ---"

export ARCH="$ARCH"
"$APPIMAGETOOL" "$APPDIR" "$OUTPUT_DIR/$APPIMAGE_NAME"

# Clean up AppDir
rm -rf "$APPDIR"

# Clean up downloaded appimagetool if we downloaded it
if [ "$APPIMAGETOOL" = "$OUTPUT_DIR/appimagetool" ]; then
  rm -f "$APPIMAGETOOL"
fi

echo ""
echo "=== AppImage created ==="
echo "Output: $OUTPUT_DIR/$APPIMAGE_NAME"
echo "Size:   $(du -sh "$OUTPUT_DIR/$APPIMAGE_NAME" | cut -f1)"
