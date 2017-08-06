#!/bin/bash

set -euo pipefail

MODULES=(
    `cat ./module-deploy-order.txt | grep -v '^#' | tail -r`
)

for MODULE in ${MODULES[*]} ; do
    (cd modules/env/$MODULE && npm run destroy -- --env $ENV_NAME --region ap-southeast-2)
done
