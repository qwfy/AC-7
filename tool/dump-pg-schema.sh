#!/usr/bin/env bash

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null && pwd )"
PROJECT_ROOT="$SCRIPT_DIR/../"

PGPASSWORD=atreehasmanyleafs pg_dump \
  --file="$PROJECT_ROOT/tool/schema.sql" \
  --format=p \
  --clean \
  --if-exists \
  --schema=public \
  --schema-only \
  --quote-all-identifiers \
  --dbname=postgres \
  --host=127.0.0.1 \
  --port=5432 \
  --username=postgres
