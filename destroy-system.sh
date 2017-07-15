#!/bin/bash

set -euo pipefail

MODULES=(
    `cat ./module-deploy-order.txt | tail -r`
)

for MODULE in ${MODULES[*]} ; do
    (cd modules/$MODULE_NAME && npm run destroy -- --env $ENV_NAME --region ap-southeast-2)
done
