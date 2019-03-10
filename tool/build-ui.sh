#!/usr/bin/env bash

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null && pwd )"
PROJECT_ROOT="$SCRIPT_DIR/../"

OLDDIR="$(pwd)"

cd "$PROJECT_ROOT/ui"

OUTPUT_JS="$1"
if [ -z "$OUTPUT_JS" ]; then
  OUTPUT_JS="js/main.js"
fi

elm make src/*.elm --output="$1"

cd "$OLDDIR"
