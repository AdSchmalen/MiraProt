#!/usr/bin/env bash
# =============================================================================
# create-dmg.sh — Build a macOS .app bundle and package it as a .dmg
# =============================================================================
# Usage:
#   ./create-dmg.sh --dist-dir ./dist --version 1.0.0 [--output-dir ./output]
#
# Expects dist-dir to contain:
#   MiraProt-launcher   (Go binary)
#   shiny-app/           (Shiny application)
#   r-portable/          (R installation)
#   r-library/           (R packages)
# =============================================================================

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
DIST_DIR=""
VERSION="dev"
OUTPUT_DIR=""
APP_NAME="MiraProt"
ARCH="$(uname -m)"

# Parse arguments
while [[ $# -gt 0 ]]; do
  case "$1" in
    --dist-dir)  DIST_DIR="$2"; shift 2 ;;
    --version)   VERSION="$2"; shift 2 ;;
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

DMG_NAME="${APP_NAME}-${VERSION}-macos-${ARCH}"
APP_BUNDLE="$OUTPUT_DIR/${APP_NAME}.app"

echo "=== Creating macOS .app bundle ==="
echo "Version: $VERSION"
echo "Arch:    $ARCH"
echo "Source:  $DIST_DIR"
echo "Output:  $OUTPUT_DIR"
echo ""

# -----------------------------------------------------------------------
# Step 1: Create .app bundle structure
# -----------------------------------------------------------------------
echo "--- Building ${APP_NAME}.app ---"

rm -rf "$APP_BUNDLE"
mkdir -p "$APP_BUNDLE/Contents/MacOS"
mkdir -p "$APP_BUNDLE/Contents/Resources"

# Copy Info.plist and substitute version
sed "s/VERSION_PLACEHOLDER/${VERSION}/g" "$SCRIPT_DIR/Info.plist" \
  > "$APP_BUNDLE/Contents/Info.plist"

# Copy launcher binary
cp "$DIST_DIR/MiraProt-launcher" "$APP_BUNDLE/Contents/MacOS/MiraProt-launcher"
chmod +x "$APP_BUNDLE/Contents/MacOS/MiraProt-launcher"

# Copy Shiny app (the launcher resolves ../Resources/app)
cp -a "$DIST_DIR/shiny-app" "$APP_BUNDLE/Contents/Resources/app"

# Copy portable R (the launcher resolves ../Resources/R)
cp -a "$DIST_DIR/r-portable" "$APP_BUNDLE/Contents/Resources/R"

# Copy R library
cp -a "$DIST_DIR/r-library" "$APP_BUNDLE/Contents/Resources/r-library"

# Copy the pre-built caches as read-only application resources. The launcher
# seeds these into the user's writable data directory on first launch.
if [ -d "$DIST_DIR/go-cache" ]; then
  cp -a "$DIST_DIR/go-cache" "$APP_BUNDLE/Contents/Resources/go-cache"
else
  mkdir -p "$APP_BUNDLE/Contents/Resources/go-cache"
fi

# Copy icon if available
if [ -f "$DIST_DIR/icon.icns" ]; then
  cp "$DIST_DIR/icon.icns" "$APP_BUNDLE/Contents/Resources/icon.icns"
elif [ -f "$SCRIPT_DIR/../../resources/icon.icns" ]; then
  cp "$SCRIPT_DIR/../../resources/icon.icns" "$APP_BUNDLE/Contents/Resources/icon.icns"
fi

echo "App bundle created: $APP_BUNDLE"
echo ""

# -----------------------------------------------------------------------
# Step 2: Create DMG
# -----------------------------------------------------------------------
echo "--- Creating DMG ---"

DMG_TEMP="$OUTPUT_DIR/${DMG_NAME}-temp.dmg"
DMG_FINAL="$OUTPUT_DIR/${DMG_NAME}.dmg"
MOUNT_DIR="/tmp/miraprot-dmg-$$"

rm -f "$DMG_TEMP" "$DMG_FINAL"

# Calculate size needed (app size + 20MB headroom)
APP_SIZE_KB=$(du -sk "$APP_BUNDLE" | cut -f1)
DMG_SIZE_KB=$((APP_SIZE_KB + 20480))

# Create temporary writable DMG
hdiutil create \
  -size "${DMG_SIZE_KB}k" \
  -fs HFS+ \
  -volname "$APP_NAME" \
  -ov \
  "$DMG_TEMP"

# Mount it
mkdir -p "$MOUNT_DIR"
hdiutil attach "$DMG_TEMP" -mountpoint "$MOUNT_DIR" -nobrowse

# Copy app bundle into the DMG
cp -a "$APP_BUNDLE" "$MOUNT_DIR/"

# Add Applications symlink for drag-and-drop install
ln -s /Applications "$MOUNT_DIR/Applications"

# Unmount
hdiutil detach "$MOUNT_DIR"
rmdir "$MOUNT_DIR" 2>/dev/null || true

# Convert to compressed read-only DMG
hdiutil convert "$DMG_TEMP" \
  -format UDZO \
  -imagekey zlib-level=9 \
  -o "$DMG_FINAL"

rm -f "$DMG_TEMP"

# Clean up app bundle (DMG contains it now)
rm -rf "$APP_BUNDLE"

echo ""
echo "=== DMG created ==="
echo "Output: $DMG_FINAL"
echo "Size:   $(du -sh "$DMG_FINAL" | cut -f1)"
