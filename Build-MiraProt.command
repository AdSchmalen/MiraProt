#!/usr/bin/env bash
ROOT="$(cd "$(dirname "$0")" && pwd)" || exit 1
cd "$ROOT" || exit 1
exec "$ROOT/portable/scripts/start-build-unix.sh" --interactive "$@"
