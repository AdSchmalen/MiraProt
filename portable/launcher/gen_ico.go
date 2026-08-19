//go:build ignore

// gen_ico.go converts MiraProt_icon.png into MiraProt.ico (multi-resolution).
// It also regenerates icon_data.go (the 32x32 system-tray icon) from the same PNG.
//
// Run from portable/launcher/ with:
//
//	go run gen_ico.go
package main

import (
	"bytes"
	"encoding/binary"
	"fmt"
	"image"
	"image/draw"
	"image/png"
	_ "image/png"
	"os"
	"path/filepath"
	"runtime"
)

// icoSize describes one image entry in the ICO file.
type icoSize struct {
	width  int
	height int
}

var sizes = []icoSize{
	{16, 16},
	{32, 32},
	{48, 48},
	{256, 256},
}

func main() {
	// Locate the project root relative to this source file's directory.
	// When invoked via "go run gen_ico.go" from portable/launcher/, __file__
	// is not available, so we resolve from the working directory.
	_, srcFile, _, ok := runtime.Caller(0)
	var projectRoot string
	if ok {
		// srcFile = .../portable/launcher/gen_ico.go
		projectRoot = filepath.Join(filepath.Dir(srcFile), "..", "..")
	} else {
		wd, _ := os.Getwd()
		projectRoot = filepath.Join(wd, "..", "..")
	}

	pngPath := filepath.Join(projectRoot, "MiraProt_icon.png")
	icoPath := filepath.Join(projectRoot, "portable", "launcher", "MiraProt.ico")
	iconDataPath := filepath.Join(projectRoot, "portable", "launcher", "icon_data.go")

	// Open source PNG
	f, err := os.Open(pngPath)
	if err != nil {
		fmt.Fprintf(os.Stderr, "ERROR: cannot open %s: %v\n", pngPath, err)
		fmt.Fprintln(os.Stderr, "Make sure MiraProt_icon.png exists in the project root.")
		os.Exit(1)
	}
	defer f.Close()

	src, _, err := image.Decode(f)
	if err != nil {
		fmt.Fprintf(os.Stderr, "ERROR: cannot decode PNG: %v\n", err)
		os.Exit(1)
	}

	// Render each size as a PNG blob.
	var pngBlobs [][]byte
	for _, sz := range sizes {
		scaled := resizeNearestNeighbor(src, sz.width, sz.height)
		var buf bytes.Buffer
		if err := png.Encode(&buf, scaled); err != nil {
			fmt.Fprintf(os.Stderr, "ERROR: cannot encode PNG at %dx%d: %v\n", sz.width, sz.height, err)
			os.Exit(1)
		}
		pngBlobs = append(pngBlobs, buf.Bytes())
	}

	// Write the ICO file.
	if err := writeICO(icoPath, sizes, pngBlobs); err != nil {
		fmt.Fprintf(os.Stderr, "ERROR: cannot write ICO: %v\n", err)
		os.Exit(1)
	}
	fmt.Printf("Written: %s (%d sizes)\n", icoPath, len(sizes))

	// Regenerate icon_data.go from the 32x32 image.
	if err := writeIconData(iconDataPath, pngBlobs[1]); err != nil {
		fmt.Fprintf(os.Stderr, "ERROR: cannot write icon_data.go: %v\n", err)
		os.Exit(1)
	}
	fmt.Printf("Written: %s (32x32 tray icon)\n", iconDataPath)
}

// writeICO writes a multi-resolution ICO file.
// Each image is stored as a raw PNG blob (Vista+ ICO format).
func writeICO(path string, szs []icoSize, blobs [][]byte) error {
	out, err := os.Create(path)
	if err != nil {
		return err
	}
	defer out.Close()

	n := len(szs)

	// ICO header: reserved(2) + type(2) + count(2)
	writeU16LE(out, 0)    // reserved
	writeU16LE(out, 1)    // type = ICO
	writeU16LE(out, n)    // image count

	// Directory entries are 16 bytes each; images follow after the directory.
	imageOffset := 6 + n*16

	for i, sz := range szs {
		w := sz.width
		h := sz.height
		if w >= 256 {
			w = 0 // 0 means 256 in ICO directory
		}
		if h >= 256 {
			h = 0
		}
		size := len(blobs[i])

		out.Write([]byte{byte(w), byte(h), 0, 0}) // width, height, colorCount, reserved
		writeU16LE(out, 1)                         // planes
		writeU16LE(out, 32)                        // bit count
		writeU32LE(out, size)
		writeU32LE(out, imageOffset)
		imageOffset += size
	}

	// Image data
	for _, blob := range blobs {
		out.Write(blob)
	}

	return nil
}

func writeU16LE(f *os.File, v int) {
	b := make([]byte, 2)
	binary.LittleEndian.PutUint16(b, uint16(v))
	f.Write(b)
}

func writeU32LE(f *os.File, v int) {
	b := make([]byte, 4)
	binary.LittleEndian.PutUint32(b, uint32(v))
	f.Write(b)
}

// resizeNearestNeighbor scales src to the given dimensions using nearest-neighbour.
func resizeNearestNeighbor(src image.Image, w, h int) image.Image {
	dst := image.NewRGBA(image.Rect(0, 0, w, h))
	srcBounds := src.Bounds()
	srcW := srcBounds.Max.X - srcBounds.Min.X
	srcH := srcBounds.Max.Y - srcBounds.Min.Y

	draw.Draw(dst, dst.Bounds(), image.Transparent, image.Point{}, draw.Src)

	for y := 0; y < h; y++ {
		for x := 0; x < w; x++ {
			srcX := srcBounds.Min.X + x*srcW/w
			srcY := srcBounds.Min.Y + y*srcH/h
			dst.Set(x, y, src.At(srcX, srcY))
		}
	}
	return dst
}

// writeIconData writes icon_data.go with the given 32x32 PNG blob.
func writeIconData(path string, pngBlob []byte) error {
	f, err := os.Create(path)
	if err != nil {
		return err
	}
	defer f.Close()

	fmt.Fprintln(f, "package main")
	fmt.Fprintln(f)
	fmt.Fprintln(f, "// iconData is the MiraProt system-tray icon (32x32 PNG).")
	fmt.Fprintln(f, "// Regenerate from MiraProt_icon.png with: go run gen_ico.go")
	fmt.Fprintf(f, "var iconData = []byte{")
	for i, b := range pngBlob {
		if i%16 == 0 {
			fmt.Fprintf(f, "\n\t")
		}
		fmt.Fprintf(f, "0x%02x, ", b)
	}
	fmt.Fprintln(f, "\n}")
	return nil
}
