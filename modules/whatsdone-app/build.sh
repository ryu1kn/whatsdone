#!/bin/bash

set -euo pipefail

SRC_DIR=./src
DIST_DIR=./dist

mkdir -p $DIST_DIR
cp -r $SRC_DIR/package.json $SRC_DIR/index.js $SRC_DIR/lib $DIST_DIR
(cd $DIST_DIR && npm install --production)
