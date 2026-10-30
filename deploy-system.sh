#!/bin/bash

set -euo pipefail

MODULES=(
    `cat ./module-deploy-order.txt | grep -v '^#'`
)

for MODULE in ${MODULES[*]} ; do
    AWS_REGION=$AWS_REGION ./deploy-module.sh --env $ENV_NAME $MODULE
done

echo -e "\n=== Promoting UI assets ==="
# Deploy the published BUILD_NUMBER ZIP with this environment's appConfig.json.
# Missing artifacts fail; deployment never rebuilds or uploads a replacement ZIP.
(cd modules/web-assets && yarn install && yarn run deploy:app )

echo
echo "========================"
echo " Deployment completed!! "
echo "========================"
echo
