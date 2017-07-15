#!/bin/bash

set -euo pipefail

MODULES__ENV=(
    `cat ./module-deploy-order--env.txt | tail -r`
)

for MODULE in ${MODULES__ENV[*]} ; do
    (cd modules/env/$MODULE_NAME && npm run destroy -- --env $ENV_NAME --region ap-southeast-2)
done
