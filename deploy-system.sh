#!/bin/bash

set -euo pipefail

MODULES=(
    `cat ./module-deploy-order.txt | grep -v '^#'`
)

for MODULE in ${MODULES[*]} ; do
    AWS_REGION=$AWS_REGION ./deploy-module.sh --env $ENV_NAME $MODULE
done

(cd modules/whatsdone-assets && yarn install && yarn run build && yarn run deploy:app )
