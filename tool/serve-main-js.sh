#!/usr/bin/env bash

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null && pwd )"
PROJECT_ROOT="$SCRIPT_DIR/../"

OUTPUT_JS="$(mktemp --suffix=.js)"
"$PROJECT_ROOT"/tool/build-ui.sh "$OUTPUT_JS" > /dev/null
echo -e "Content-type: application/javascript\n\n"
cat "$OUTPUT_JS"
rm "$OUTPUT_JS"
