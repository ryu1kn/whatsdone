#!/bin/bash

set -euo pipefail

MODULES__NO_ENV=(
    `cat ./module-deploy-order--no-env.txt`
)
MODULES__ENV=(
    `cat ./module-deploy-order--env.txt`
)

for MODULE in ${MODULES__NO_ENV[*]} ; do
    (cd modules/no-env/$MODULE && npm run deploy -- --region ap-southeast-2)
done

for MODULE in ${MODULES__ENV[*]} ; do
    (cd modules/env/$MODULE && npm run deploy -- --env $ENV_NAME --region ap-southeast-2)
done
