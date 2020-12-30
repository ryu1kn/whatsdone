#!/bin/bash

set -euo pipefail

build_dir=built
dist_dir="$npm_package_config_BUILD_DIR_NAME"

mkdir -p "$dist_dir"
cp -r package.json "$build_dir/index.js" "$build_dir/lib" "$dist_dir"
(cd "$dist_dir" && yarn install --production)
