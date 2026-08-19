from pathlib import Path


root = Path(__file__).resolve().parents[2]
version_source = (root / "R" / "version_info.R").read_text(encoding="utf-8")
ui_source = (root / "R" / "ui.R").read_text(encoding="utf-8")

assert 'MIRAPROT_VERSION_BASE <- "1.0"' in version_source
assert 'c("rev-list", "--count", "HEAD")' in version_source
assert 'c("log", "-1", "--format=%cs")' in version_source
assert 'version_info <- miraprot_version_info()' in ui_source
assert 'p("Current Version: ", version_info$version)' in ui_source
assert 'p("Commit: ", version_info$commit)' in ui_source
assert 'p("Last Updated: ", version_info$last_updated)' in ui_source
assert "Sys.Date()" not in ui_source

print("Version information static checks passed")
