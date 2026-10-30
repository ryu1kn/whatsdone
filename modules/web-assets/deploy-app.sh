#!/bin/bash

set -euo pipefail

for name in ENV_NAME BUILD_NUMBER npm_package_config_BUILD_DIR; do
    if [[ -z "${!name:-}" ]]; then
        echo "Environment variable $name is not set" >&2
        exit 1
    fi
done

# Validate environment configuration before downloading or changing website objects.
node - "$ENV_NAME" <<'JS'
const env = process.argv[2];
const config = require(`./module-config/${env}`);
for (const name of ['bucketName', 'artifactBucket', 'artifactBasePath']) {
  if (typeof config[name] !== 'string' || !config[name].trim()) {
    throw new Error(`Missing deployment setting ${name} for ${env}`);
  }
}
const app = require(`./app-config/${env}.json`);
for (const name of ['API_ORIGIN', 'REGION', 'IDENTITY_POOL_ID', 'USER_POOL_ID', 'CLIENT_ID']) {
  if (typeof app[name] !== 'string' || !app[name].trim()) {
    throw new Error(`Missing app setting ${name} for ${env}`);
  }
}
new URL(app.API_ORIGIN);
JS

DEPLOY_BUCKET="$(node -p "require('./module-config/$ENV_NAME').bucketName")"
ARTIFACT_BUCKET_NAME="$(node -p "require('./module-config/$ENV_NAME').artifactBucket")"
ARTIFACT_BASE_PATH="$(node -p "require('./module-config/$ENV_NAME').artifactBasePath")"
BUILD_DIR="$npm_package_config_BUILD_DIR"
ARTIFACT_DOWNLOAD_PATH="$BUILD_DIR/$BUILD_NUMBER.zip"
ARTIFACT_UNZIPPED_PATH="$BUILD_DIR/$BUILD_NUMBER"

mkdir -p "$BUILD_DIR"
ARTIFACT_UNZIPPED_PATH="$(mktemp -d "$BUILD_DIR/deploy.XXXXXX")"
trap 'rm -rf "$ARTIFACT_UNZIPPED_PATH"' EXIT
aws s3 cp "s3://$ARTIFACT_BUCKET_NAME/$ARTIFACT_BASE_PATH/$BUILD_NUMBER.zip" "$ARTIFACT_DOWNLOAD_PATH"
unzip "$ARTIFACT_DOWNLOAD_PATH" -d "$ARTIFACT_UNZIPPED_PATH"
cp "./app-config/$ENV_NAME.json" "$ARTIFACT_UNZIPPED_PATH/appConfig.json"
aws s3 sync --delete "$ARTIFACT_UNZIPPED_PATH" "s3://$DEPLOY_BUCKET"    # S3 sync sometimes fails to update existing files. Here, using for deletion
aws s3 cp --recursive "$ARTIFACT_UNZIPPED_PATH" "s3://$DEPLOY_BUCKET"
