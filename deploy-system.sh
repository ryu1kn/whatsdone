#!/bin/bash

set -euo pipefail

MODULES=(
    `cat ./module-deploy-order.txt | grep -v '^#'`
)

for MODULE in ${MODULES[*]} ; do
    (cd modules/env/$MODULE && npm run deploy -- --env $ENV_NAME --region $AWS_REGION)
done

(cd modules/env/whatsdone-assets && npm install && npm run build && npm run deploy:app )
