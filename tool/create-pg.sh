#!/usr/bin/env bash

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null && pwd )"
PROJECT_ROOT="$SCRIPT_DIR/../"
DATA_ROOT="$PROJECT_ROOT/pg-data/"


docker run \
  -it \
  --name ac7-pg \
  --env POSTGRES_PASSWORD=atreehasmanyleafs \
  --env PGDATA=/pgdata \
  --volume "$DATA_ROOT":/pgdata \
  --publish 5432:5432 \
  --user=$(id -u):$(id -g) \
  -detach \
  postgres:11.1
