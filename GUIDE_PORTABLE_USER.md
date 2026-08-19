# MiraProt Standalone Edition — User Guide

This guide explains how to install and use MiraProt as a standalone desktop
application. You do **not** need R, RStudio, or Docker. Everything is bundled
into a single download that runs on your computer.

---

## Table of Contents

1. [What is MiraProt Standalone?](#1-what-is-miraprot-standalone)
2. [System Requirements](#2-system-requirements)
3. [Installing MiraProt](#3-installing-miraprot)
4. [Starting MiraProt](#4-starting-miraprot)
5. [Using MiraProt](#5-using-miraprot)
6. [Stopping MiraProt](#6-stopping-miraprot)
7. [Starting Again Later](#7-starting-again-later)
8. [Where MiraProt Stores Data](#8-where-miraprot-stores-data)
9. [Common Problems and Solutions](#9-common-problems-and-solutions)
10. [Features That Need an Internet Connection](#10-features-that-need-an-internet-connection)

---

## 1. What is MiraProt Standalone?

MiraProt Standalone is a self-contained desktop version of MiraProt. It
includes:

- The MiraProt application itself
- A portable copy of R 4.6.0
- All 98 required R packages (pre-compiled)
- A small launcher program that manages everything

When you start MiraProt, the launcher opens the application in your default
web browser. The app runs entirely on your computer — your data never leaves
your machine (except for features that explicitly connect to online databases,
see [section 10](#10-features-that-need-an-internet-connection)).

A system tray icon appears next to your clock, giving you quick access to
common actions (open in browser, view logs, quit).

---

## 2. System Requirements

| | Minimum | Recommended |
|---|---|---|
| **Disk space** | 2 GB | 4 GB (with organism caches) |
| **RAM** | 4 GB | 8 GB |
| **Internet** | Not required for most features | Required for GO, STRING, biomaRt |

### Supported Operating Systems

| OS | Minimum Version | Architecture |
|---|---|---|
| Windows | 10 (64-bit) | x86_64 (AMD/Intel) |
| macOS | 11.0 (Big Sur) | Intel or Apple Silicon (M1/M2/M3/M4) |
| Linux | Ubuntu 20.04 or equivalent | x86_64 (AMD/Intel) |

---

## 3. Installing MiraProt

Go to the MiraProt releases page on GitHub:

**https://github.com/AdSchmalen/MiraProt/releases**

Download the file that matches your operating system. There are two options
per platform: an **installer** (recommended) and a **portable archive**.

### Windows

**Option A — Installer (recommended)**

1. Download `MiraProt-vX.Y.Z-windows-setup.exe`.
2. Double-click the downloaded file.
3. If Windows SmartScreen shows a warning:
   - Click **"More info"**
   - Click **"Run anyway"**
4. Follow the installer steps. The defaults are fine.
   - Default install location: `C:\Program Files\MiraProt`
   - You can optionally create a desktop shortcut.
5. Click **Finish**. MiraProt is installed.

**Option B — Portable (no installation needed)**

1. Download `miraprot-windows-amd64.zip`.
2. Right-click the file and select **"Extract All..."**.
3. Choose a folder (e.g. `C:\MiraProt`).
4. Open the extracted folder and double-click `MiraProt-launcher.exe`.

### macOS

1. Download `MiraProt-vX.Y.Z-macos-arm64.dmg` (Apple Silicon) or
   `MiraProt-vX.Y.Z-macos-amd64.dmg` (Intel).

   Not sure which one? Click the Apple icon in the top-left corner of your
   screen and select **"About This Mac"**. If it says "Apple M..." you need
   the arm64 version. If it says "Intel" you need the amd64 version.

2. Double-click the downloaded `.dmg` file.

3. Drag **MiraProt** into the **Applications** folder.

4. Open **MiraProt** from your Applications folder.

5. macOS may show: *"MiraProt can't be opened because it is from an
   unidentified developer."* To fix this:
   - Right-click (or Control-click) MiraProt in Applications
   - Select **"Open"** from the context menu
   - Click **"Open"** in the dialog
   - You only need to do this the first time.

### Linux

**Option A — AppImage (recommended)**

1. Download `MiraProt-vX.Y.Z-linux-amd64.AppImage`.
2. Make it executable:
   ```bash
   chmod +x MiraProt-*.AppImage
   ```
3. Double-click the file, or run it from a terminal:
   ```bash
   ./MiraProt-vX.Y.Z-linux-amd64.AppImage
   ```

**Option B — Portable archive**

1. Download `miraprot-linux-amd64.tar.gz`.
2. Extract it:
   ```bash
   tar xzf miraprot-linux-amd64.tar.gz -C ~/MiraProt
   ```
3. Run the launcher:
   ```bash
   ~/MiraProt/MiraProt-launcher
   ```

---

## 4. Starting MiraProt

Launch MiraProt the same way you would open any application:

| OS | How to start |
|---|---|
| Windows (installed) | Start Menu → **MiraProt**, or desktop shortcut |
| Windows (portable) | Double-click `MiraProt-launcher.exe` |
| macOS | Applications → **MiraProt** |
| Linux (AppImage) | Double-click the `.AppImage` file |
| Linux (portable) | Run `./MiraProt-launcher` in a terminal |

### What happens when you start

1. A **system tray icon** appears near your clock (a small MiraProt icon).
2. The launcher starts R and the Shiny server in the background.
3. After a few seconds, your **web browser opens** automatically to:
   ```
   http://127.0.0.1:3838
   ```
4. MiraProt appears in the browser, ready to use.

If the browser does not open automatically, open it yourself and go to
`http://127.0.0.1:3838`.

---

## 5. Using MiraProt

Once MiraProt opens in your browser, you can use it just like any website.

- **Upload your data** using the Data Wizard module (the first tab).
  MiraProt accepts Excel files (.xlsx) and CSV files.
- **Navigate between modules** using the tabs at the top (PCA, Volcano,
  Heatmap, Venn, GO, GSEA, STRING, etc.).
- **Download results** using the download buttons within each module. You
  can export plots as PNG or SVG images and data as Excel files.

MiraProt runs locally on your computer — your data never leaves your
machine (except when you use features that explicitly connect to online
databases, see [section 10](#10-features-that-need-an-internet-connection)).

---

## 6. Stopping MiraProt

There are three ways to stop MiraProt:

1. **Close the browser tab.** The app detects this and shuts down
   automatically within a few seconds.

2. **Right-click the system tray icon** (the MiraProt icon near your clock)
   and select **"Quit MiraProt"**.

3. **Press Ctrl+C** in the terminal window (if you started from a terminal).

---

## 7. Starting Again Later

Just launch MiraProt again the same way you did before. It starts within a
few seconds since everything is already installed.

MiraProt checks for updates automatically each time it starts. If a newer
version is available, you will see a message like:

```
[UPDATE] A new version is available: v1.2.0 (you have v1.1.0).
         Download at: https://github.com/AdSchmalen/MiraProt/releases/tag/v1.2.0
```

To update, download the new version from the link and install it over the
existing one (or replace the portable folder).

---

## 8. Where MiraProt Stores Data

MiraProt creates a small data folder for logs and caches. This folder is
separate from the application itself and is **not** removed when you
uninstall.

| OS | Data folder |
|---|---|
| Windows | `C:\Users\YourName\AppData\Local\MiraProt` |
| macOS | `~/Library/Application Support/MiraProt` |
| Linux | `~/.local/share/MiraProt` |

Inside that folder:

| Subfolder | What it contains |
|---|---|
| `logs/` | Daily log files (`miraprot-2025-01-15.log`). Automatically cleaned after 7 days. |
| `cache/annotation_cache/` | AnnotationHub organism databases (downloaded on first GO analysis). |
| `cache/go_cache/` | Gene Ontology cache files. |

**If something goes wrong**, the log files are the best place to look.
Open the latest file in `logs/` with any text editor, or right-click the
system tray icon and select **"View Log File"**.

To **reset everything**, delete the data folder listed above and restart
MiraProt. It will recreate the folder with fresh defaults.

---

## 9. Common Problems and Solutions

### The browser opens but shows "This site can't be reached"

MiraProt may still be starting up. Wait 10-15 seconds and refresh the page
(press F5). If it still does not work after 30 seconds, check the log file
for errors (see [section 8](#8-where-miraprot-stores-data)).

### "Another MiraProt instance is already running"

Only one copy of MiraProt can run at a time. Either close the other
instance first, or if no other instance is actually running, delete the lock
file and try again:

| OS | Lock file to delete |
|---|---|
| Windows | `C:\Users\YourName\AppData\Local\MiraProt\launcher.lock` |
| macOS | `~/Library/Application Support/MiraProt/launcher.lock` |
| Linux | `~/.local/share/MiraProt/launcher.lock` |

### Port 3838 is already in use

Another program on your computer is using the same port. MiraProt
automatically tries the next available port (up to port 4838), but if all
ports are taken:

- Close the other program and try again.
- Or start MiraProt from a terminal with a different port:
  ```
  MiraProt-launcher --port 5000
  ```
  Then open `http://127.0.0.1:5000` in your browser.

### Windows: SmartScreen blocks the installer or launcher

Click **"More info"**, then click **"Run anyway"**. This warning appears
because the application is not signed with a commercial code-signing
certificate.

### macOS: "MiraProt can't be opened"

See the Gatekeeper instructions in [section 3](#3-installing-miraprot)
under macOS. Right-click the app, select "Open", and confirm.

### Linux: AppImage does nothing when double-clicked

Make the file executable first:
```bash
chmod +x MiraProt-*.AppImage
```
Then double-click it again, or run it from a terminal.

### The first GO analysis is very slow

The first time you run a Gene Ontology analysis for a given organism,
MiraProt downloads the organism database from AnnotationHub (requires
internet). This download can take a few minutes. The database is cached
locally, so the same organism will load much faster next time.

### System tray icon does not appear (Linux)

The system tray requires GTK 3. On Ubuntu/Debian, install:
```bash
sudo apt-get install -y libgtk-3-0 libayatana-appindicator3-1
```
If your desktop environment does not support system tray icons, start
MiraProt from a terminal — it works the same way, just without the tray
icon.

---

## 10. Features That Need an Internet Connection

Most of MiraProt works completely offline. However, some modules need to
connect to online databases:

| Module | What it connects to | When |
|---|---|---|
| Gene Ontology (GO) | AnnotationHub (Bioconductor) | First time you analyze a new organism. The downloaded database is saved locally so you do not need internet for the same organism again. |
| STRING Network | STRING database (string-db.org) | Every time you run a STRING analysis. |
| Annotation | Ensembl (via biomaRt) | When you query gene/protein annotations. |

All other modules work fully offline:
- Data Wizard (import, filtering, normalization, batch correction)
- PCA
- Volcano plots
- Heatmaps
- Venn diagrams
- Dotplots
- GSEA (uses bundled gene set files)
- Data export (Excel, PNG, SVG)
