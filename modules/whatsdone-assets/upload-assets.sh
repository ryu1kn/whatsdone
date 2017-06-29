#!/bin/bash

set -euo pipefail

DEPLOY_CONFIG=$1
BUCKET_NAME=`echo $DEPLOY_CONFIG | jq .bucketName --raw-output`
DIST_DIR=$npm_package_config_buildDir

npm run build
aws s3 sync $DIST_DIR s3://$BUCKET_NAME