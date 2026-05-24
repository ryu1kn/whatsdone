#!/bin/bash

set -euo pipefail

DEPLOY_BUCKET="$(node -p "require('./module-config/$ENV_NAME').bucketName")"
ARTIFACT_BUCKET_NAME="$(node -p "require('./module-config/$ENV_NAME').artifactBucket")"
ARTIFACT_BASE_PATH="$(node -p "require('./module-config/$ENV_NAME').artifactBasePath")"
BUILD_DIR="$npm_package_config_BUILD_DIR"
ARTIFACT_DOWNLOAD_PATH="$BUILD_DIR/$BUILD_NUMBER.zip"
ARTIFACT_UNZIPPED_PATH="$BUILD_DIR/$BUILD_NUMBER"

rm -rf "$BUILD_DIR"
aws s3 cp "s3://$ARTIFACT_BUCKET_NAME/$ARTIFACT_BASE_PATH/$BUILD_NUMBER.zip" "$ARTIFACT_DOWNLOAD_PATH"
unzip "$ARTIFACT_DOWNLOAD_PATH" -d "$ARTIFACT_UNZIPPED_PATH"
cp "./app-config/$ENV_NAME.json" "$ARTIFACT_UNZIPPED_PATH/appConfig.json"
aws s3 sync --delete "$ARTIFACT_UNZIPPED_PATH" "s3://$DEPLOY_BUCKET"    # S3 sync sometimes fails to update existing files. Here, using for deletion
aws s3 cp --recursive "$ARTIFACT_UNZIPPED_PATH" "s3://$DEPLOY_BUCKET"
