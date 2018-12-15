#!/usr/bin/env bash

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null && pwd )"
PROJECT_ROOT="$SCRIPT_DIR/../"

rsync \
  --progress \
  --recursive \
  "$PROJECT_ROOT"/* \
  incomplete@weierstrass:/home/incomplete/AC-7/
