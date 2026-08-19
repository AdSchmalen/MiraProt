"""Static checks for session save/restore dot-prefixed helper calls."""
from __future__ import annotations

import re
from pathlib import Path

SESSION_SAVE_RESTORE_FILES = (
    Path("R/session_save_restore/session_save_restore_core_helpers.R"),
    Path("R/session_save_restore/session_save_restore_orchestration.R"),
    Path("R/session_save_restore/session_save_restore_module_registration.R"),
    Path("R/session_save_restore.R"),
)

DOT_CALL_RE = re.compile(r"\.([A-Za-z0-9_]+)\s*\(")
DOT_FUNCTION_DEF_RE = re.compile(r"\.([A-Za-z0-9_]+)\s*<-\s*function\b")
MODENV_IMPORT_RE = re.compile(r"\.([A-Za-z0-9_]+)\s*<-\s*modEnv\$\.([A-Za-z0-9_]+)\b")

# Base R / imported S3 methods and non-helper member calls that are intentionally
# dot-prefixed but are not project-local session save/restore helper functions.
ALLOWLISTED_DOT_CALLS = {
    "GlobalEnv",
    "POSIXct",
    "appendChild",
    "array",
    "atomic",
    "character",
    "createElement",
    "draw",
    "env",
    "environment",
    "error",
    "execCommand",
    "exists",
    "exit",
    "focus",
    "frame",
    "function",
    "getenv",
    "grabExpr",
    "int",
    "integer",
    "list",
    "log",
    "matrix",
    "max",
    "na",
    "null",
    "numeric",
    "object",
    "packages",
    "path",
    "raw",
    "removeChild",
    "scrollTo",
    "select",
    "size",
    "source",
    "then",
    "time",
    "writeText",
    # Local variable that stores a function from a validation table, not a helper.
    "sr_const_validator",
}


def session_save_restore_text() -> str:
    return "\n".join(path.read_text() for path in SESSION_SAVE_RESTORE_FILES)


def dot_calls(text: str) -> set[str]:
    return set(DOT_CALL_RE.findall(text))


def dot_function_definitions(text: str) -> set[str]:
    return set(DOT_FUNCTION_DEF_RE.findall(text))


def modenv_dot_imports(text: str) -> set[str]:
    imports = set()
    for local_name, modenv_name in MODENV_IMPORT_RE.findall(text):
        imports.add(local_name)
        imports.add(modenv_name)
    return imports


def unresolved_dot_helper_calls(text: str) -> set[str]:
    return dot_calls(text) - dot_function_definitions(text) - modenv_dot_imports(text) - ALLOWLISTED_DOT_CALLS


def test_session_save_restore_dot_helpers_are_defined_or_imported_from_modenv() -> None:
    unresolved = unresolved_dot_helper_calls(session_save_restore_text())
    assert not unresolved, (
        "Unresolved session save/restore dot-helper call(s): "
        + ", ".join(f".{name}()" for name in sorted(unresolved))
    )


def test_is_valid_pair_regression_is_caught_as_unresolved_project_helper() -> None:
    text = """
    .defined_helper <- function(x) x
    caller <- function(cached) .is_valid_pair(cached) && .defined_helper(cached)
    """
    assert "is_valid_pair" in unresolved_dot_helper_calls(text)


if __name__ == "__main__":
    test_session_save_restore_dot_helpers_are_defined_or_imported_from_modenv()
    test_is_valid_pair_regression_is_caught_as_unresolved_project_helper()
    print("Session save/restore dot-helper static checks passed")
