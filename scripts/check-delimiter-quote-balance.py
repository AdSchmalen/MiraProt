#!/usr/bin/env python3
"""Lightweight delimiter and quote balance check for app-critical R files.

This is not a replacement for `Rscript -e 'parse(...)'`. It is a fallback static
check for agents/environments that cannot run real R.
"""
from __future__ import annotations

from pathlib import Path
import sys

TARGETS = [
    Path("R/session_save_restore/session_save_restore_orchestration.R"),
    Path("R/session_save_restore/session_save_restore_core_helpers.R"),
    Path("R/session_save_restore/session_save_restore_module_registration.R"),
    Path("R/session_save_restore.R"),
    Path("modules/Heatmap/Heatmap_download.R"),
]

PAIRS = {"(": ")", "[": "]", "{": "}"}
CLOSERS = {v: k for k, v in PAIRS.items()}


def check_file(path: Path) -> list[str]:
    text = path.read_text(encoding="utf-8")
    errors: list[str] = []
    stack: list[tuple[str, int, int]] = []
    quote: str | None = None
    escaped = False

    for line_no, raw_line in enumerate(text.splitlines(), start=1):
        col = 0
        for ch in raw_line:
            col += 1
            if quote is not None:
                if escaped:
                    escaped = False
                elif ch == "\\":
                    escaped = True
                elif ch == quote:
                    quote = None
                continue

            if ch == "#":
                break
            if ch in ('"', "'", "`"):
                quote = ch
            elif ch in PAIRS:
                stack.append((ch, line_no, col))
            elif ch in CLOSERS:
                if not stack or stack[-1][0] != CLOSERS[ch]:
                    errors.append(f"{path}:{line_no}:{col}: unmatched closing {ch!r}")
                else:
                    stack.pop()


    if quote is not None:
        errors.append(f"{path}:{len(text.splitlines())}:0: unterminated {quote!r} string at EOF")
    for opener, opener_line, opener_col in reversed(stack):
        errors.append(f"{path}:{opener_line}:{opener_col}: unmatched opening {opener!r}")
    return errors


def main() -> int:
    missing = [str(path) for path in TARGETS if not path.exists()]
    if missing:
        print("Missing required files:\n" + "\n".join(missing), file=sys.stderr)
        return 1

    errors = [error for path in TARGETS for error in check_file(path)]
    if errors:
        print("Delimiter/quote balance check failed:", file=sys.stderr)
        print("\n".join(errors), file=sys.stderr)
        return 1

    print("Delimiter/quote balance check passed")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
