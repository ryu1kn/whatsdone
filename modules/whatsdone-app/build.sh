#!/bin/bash

set -euo pipefail

app_dir=./src
app_build_dir=$app_dir/built
dist_dir=$npm_package_config_BUILD_DIR

(cd $app_dir && yarn run build)

mkdir -p $dist_dir
cp -r $app_dir/package.json $app_dir/tsconfig.json $app_build_dir/index.js $app_build_dir/lib $dist_dir
(cd $dist_dir && yarn install --production)
