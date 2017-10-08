#!/bin/bash

set -euo pipefail

MODULES=(
    `cat ./module-deploy-order.txt | grep -v '^#'`
)

for MODULE in ${MODULES[*]} ; do
    (cd modules/$MODULE && yarn run deploy --env $ENV_NAME --region $AWS_REGION)
done

(cd modules/whatsdone-assets && yarn install && yarn run build && yarn run deploy:app )
