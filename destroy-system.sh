#!/bin/bash

set -euo pipefail

MODULES=(
    `cat ./module-deploy-order.txt | grep -v '^#' | tail -r`
)

for MODULE in ${MODULES[*]} ; do
    (cd modules/$MODULE && yarn run destroy --env $ENV_NAME --region ap-southeast-2)
done

echo
echo "==========================="
echo " Un-deployment completed!! "
echo "==========================="
echo
