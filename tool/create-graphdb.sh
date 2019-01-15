#!/usr/bin/env bash

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null && pwd )"
PROJECT_ROOT="$SCRIPT_DIR/../"
NEO4j_DATA_ROOT="$PROJECT_ROOT/neo4j-data/"

mkdir -p "$NEO4j_DATA_ROOT/data"
mkdir -p "$NEO4j_DATA_ROOT/logs"

docker run \
  --name ac7-neo4j \
  --env=NEO4J_ACCEPT_LICENSE_AGREEMENT=yes \
  --publish=7473:7473 \
  --publish=7474:7474 \
  --publish=7687:7687 \
  --user=$(id -u):$(id -g) \
  --volume="$NEO4j_DATA_ROOT/data":/data \
  --volume="$NEO4j_DATA_ROOT/logs":/logs \
  neo4j:3.5.1-enterprise
