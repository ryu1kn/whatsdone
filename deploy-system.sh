#!/bin/bash

set -euo pipefail

MODULES=(
    whatsdone-config
    whatsdone-env
    whatsdone-assets
    whatsdone-web-cache
    whatsdone-app
    whatsdone-api
    whatsdone-gateway
)

for MODULE in ${MODULES[*]} ; do
    ./deploy-module.sh -m $MODULE -- --env $ENV_NAME --region ap-southeast-2
done
