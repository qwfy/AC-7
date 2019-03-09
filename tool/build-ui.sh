#!/usr/bin/env bash

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null && pwd )"
PROJECT_ROOT="$SCRIPT_DIR/../"

OLDDIR="$(pwd)"

cd "$PROJECT_ROOT/ui"

elm make src/*.elm --output=index.html

cd "$OLDDIR"
