#!/bin/bash

set -euo pipefail

MODULES=(
    `cat ./module-deploy-order.txt`
)

for MODULE in ${MODULES[*]} ; do
    (cd modules/$MODULE_NAME && npm run deploy -- --env $ENV_NAME --region ap-southeast-2)
done
