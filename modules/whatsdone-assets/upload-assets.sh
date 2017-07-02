#!/bin/bash

set -euo pipefail

DEPLOY_CONFIG=$1
BUCKET_NAME=`echo $DEPLOY_CONFIG | jq .bucketName --raw-output`
APP_CONFIG_DIR=./app-config
DIST_DIR=$npm_package_config_buildDir

main() {
  npm run build

  APP_CONFIG_PATH=`getConfigName`
  cp $APP_CONFIG_PATH $DIST_DIR/app.config.js
  aws s3 sync $DIST_DIR s3://$BUCKET_NAME
}

getConfigName() {
  local CONFIG_FILE_PATH=$APP_CONFIG_DIR/$KUMO_ENV.js
  if [ -f $CONFIG_FILE_PATH ] ; then
    echo $CONFIG_FILE_PATH
  else
    echo $APP_CONFIG_DIR/default.js
  fi
}

main "$@"
