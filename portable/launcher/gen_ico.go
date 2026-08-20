//go:build ignore

// gen_ico.go converts MiraProt_icon.png into MiraProt.ico (multi-resolution).
// It also regenerates the platform-specific system-tray icon data from the same PNG.
//
// Write disposable build inputs with:
//
//	go run gen_ico.go -output-dir <directory>
//
// Maintainers can intentionally refresh the committed copies with -write-source.
package main

import (
	"bytes"
	"encoding/binary"
	"flag"
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
	outputDir := flag.String("output-dir", "", "directory for generated launcher icon artifacts")
	writeSource := flag.Bool("write-source", false, "overwrite the committed launcher icon artifacts")
	flag.Parse()
	if flag.NArg() != 0 || (*outputDir == "" && !*writeSource) || (*outputDir != "" && *writeSource) {
		fmt.Fprintln(os.Stderr, "ERROR: specify exactly one of -output-dir <directory> or -write-source")
		flag.Usage()
		os.Exit(2)
	}

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
	projectRoot, err := filepath.Abs(projectRoot)
	if err != nil {
		fmt.Fprintf(os.Stderr, "ERROR: cannot resolve project root: %v\n", err)
		os.Exit(1)
	}
	targetDir := *outputDir
	if *writeSource {
		targetDir = filepath.Join(projectRoot, "portable", "launcher")
	} else {
		targetDir, err = filepath.Abs(targetDir)
		if err != nil {
			fmt.Fprintf(os.Stderr, "ERROR: cannot resolve output directory: %v\n", err)
			os.Exit(1)
		}
	}
	if err := os.MkdirAll(targetDir, 0755); err != nil {
		fmt.Fprintf(os.Stderr, "ERROR: cannot create output directory %s: %v\n", targetDir, err)
		os.Exit(1)
	}
	icoPath := filepath.Join(targetDir, "MiraProt.ico")
	windowsIconDataPath := filepath.Join(targetDir, "icon_data_windows.go")
	nonWindowsIconDataPath := filepath.Join(targetDir, "icon_data_nonwindows.go")

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

	// Build and validate the complete ICO before writing or embedding it.
	icoData, err := makeICO(sizes, pngBlobs)
	if err != nil {
		fmt.Fprintf(os.Stderr, "ERROR: cannot generate ICO: %v\n", err)
		os.Exit(1)
	}
	if err := validateICO(icoData, len(sizes)); err != nil {
		fmt.Fprintf(os.Stderr, "ERROR: generated ICO is invalid: %v\n", err)
		os.Exit(1)
	}
	if err := os.WriteFile(icoPath, icoData, 0644); err != nil {
		fmt.Fprintf(os.Stderr, "ERROR: cannot write ICO: %v\n", err)
		os.Exit(1)
	}
	fmt.Printf("Written: %s (%d sizes)\n", icoPath, len(sizes))

	// Embed the complete ICO for Windows tray APIs.
	if err := writeIconData(windowsIconDataPath, "windows", "multi-resolution ICO", icoData); err != nil {
		fmt.Fprintf(os.Stderr, "ERROR: cannot write icon_data_windows.go: %v\n", err)
		os.Exit(1)
	}
	fmt.Printf("Written: %s (Windows tray icon)\n", windowsIconDataPath)

	// Embed the existing 32x32 PNG for non-Windows tray APIs.
	if !bytes.HasPrefix(pngBlobs[1], []byte{0x89, 'P', 'N', 'G', 0x0d, 0x0a, 0x1a, 0x0a}) {
		fmt.Fprintln(os.Stderr, "ERROR: generated non-Windows tray data is not PNG")
		os.Exit(1)
	}
	if err := writeIconData(nonWindowsIconDataPath, "!windows", "32x32 PNG", pngBlobs[1]); err != nil {
		fmt.Fprintf(os.Stderr, "ERROR: cannot write icon_data_nonwindows.go: %v\n", err)
		os.Exit(1)
	}
	fmt.Printf("Written: %s (non-Windows tray icon)\n", nonWindowsIconDataPath)
}

// makeICO creates a multi-resolution ICO file.
// Each image is stored as a raw PNG blob (Vista+ ICO format).
func makeICO(szs []icoSize, blobs [][]byte) ([]byte, error) {
	if len(szs) == 0 || len(szs) != len(blobs) {
		return nil, fmt.Errorf("image sizes and blobs do not match")
	}
	var out bytes.Buffer

	n := len(szs)

	// ICO header: reserved(2) + type(2) + count(2)
	writeU16LE(&out, 0) // reserved
	writeU16LE(&out, 1) // type = ICO
	writeU16LE(&out, n) // image count

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
		writeU16LE(&out, 1)                       // planes
		writeU16LE(&out, 32)                      // bit count
		writeU32LE(&out, size)
		writeU32LE(&out, imageOffset)
		imageOffset += size
	}

	// Image data
	for _, blob := range blobs {
		out.Write(blob)
	}

	return out.Bytes(), nil
}

func validateICO(data []byte, expectedEntries int) error {
	if len(data) < 6+expectedEntries*16 || !bytes.Equal(data[:4], []byte{0, 0, 1, 0}) {
		return fmt.Errorf("missing ICO header or directory")
	}
	count := int(binary.LittleEndian.Uint16(data[4:6]))
	if count != expectedEntries || count == 0 {
		return fmt.Errorf("got %d image entries, want %d", count, expectedEntries)
	}
	for i := 0; i < count; i++ {
		entry := data[6+i*16 : 6+(i+1)*16]
		size := int(binary.LittleEndian.Uint32(entry[8:12]))
		offset := int(binary.LittleEndian.Uint32(entry[12:16]))
		if size < 8 || offset < 6+count*16 || offset > len(data)-size {
			return fmt.Errorf("image entry %d has invalid size or offset", i)
		}
		if !bytes.HasPrefix(data[offset:offset+size], []byte{0x89, 'P', 'N', 'G'}) {
			return fmt.Errorf("image entry %d is not PNG data", i)
		}
	}
	return nil
}

type byteWriter interface {
	Write([]byte) (int, error)
}

func writeU16LE(f byteWriter, v int) {
	b := make([]byte, 2)
	binary.LittleEndian.PutUint16(b, uint16(v))
	f.Write(b)
}

func writeU32LE(f byteWriter, v int) {
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

// writeIconData writes platform-specific compiled-in tray icon data.
func writeIconData(path, buildConstraint, description string, icon []byte) error {
	f, err := os.Create(path)
	if err != nil {
		return err
	}
	defer f.Close()

	fmt.Fprintf(f, "//go:build %s\n\n", buildConstraint)
	fmt.Fprintln(f, "package main")
	fmt.Fprintln(f)
	fmt.Fprintf(f, "// iconData is the MiraProt system-tray icon (%s).\n", description)
	fmt.Fprintln(f, "// Regenerate from MiraProt_icon.png with: go run gen_ico.go -write-source")
	fmt.Fprintf(f, "var iconData = []byte{")
	for i, b := range icon {
		if i%16 == 0 {
			fmt.Fprintf(f, "\n\t")
		}
		fmt.Fprintf(f, "0x%02x, ", b)
	}
	fmt.Fprintln(f, "\n}")
	return nil
}
