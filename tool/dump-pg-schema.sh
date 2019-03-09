#!/usr/bin/env bash

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null && pwd )"
PROJECT_ROOT="$SCRIPT_DIR/../"

PGPASSWORD=atreehasmanyleafs pg_dumpall \
  --file="$PROJECT_ROOT/tool/pg-schema.sql" \
  --clean \
  --schema-only \
  --if-exists \
  --quote-all-identifiers \
  --host=127.0.0.1 \
  --port=5432 \
  --dbname='dbname=postgres' \
  --username=postgres 
