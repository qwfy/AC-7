#!/usr/bin/env bash

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null && pwd )"
PROJECT_ROOT="$SCRIPT_DIR/../"

# pull the image
# docker pull neo4j:3.5.1-enterprise

docker run \
  --name ac7-neo4j \
  --env=NEO4J_ACCEPT_LICENSE_AGREEMENT=yes \
  --publish=7473:7473 \
  --publish=7474:7474 \
  --publish=7687:7687 \
  --user=$(id -u):$(id -g) \
  --volume="$PROJECT_ROOT/neo4j-data/data":/data \
  --volume="$PROJECT_ROOT/neo4j-data/logs":/logs \
  neo4j:3.5.1-enterprise
