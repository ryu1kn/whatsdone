#!/bin/bash

set -euo pipefail

MODULES=(
    `cat ./module-deploy-order.txt | grep -v '^#'`
)

for MODULE in ${MODULES[*]} ; do
    AWS_REGION=$AWS_REGION ./deploy-module.sh --env $ENV_NAME $MODULE
done

echo -e "\n=== Promoting UI assets ==="
# Promote the single web-assets bundle built once by the build-artifacts CI job
# (see modules/web-assets/package.json `publish-artifact`). deploy:app downloads
# that exact artifact, layers this environment's appConfig.json on top, and syncs
# it to the env bucket. It does NOT rebuild, so prod ships the same bytes CI built
# and e2e-validated. Fails fast if the artifact is missing.
(cd modules/web-assets && yarn install && yarn run deploy:app )

echo
echo "========================"
echo " Deployment completed!! "
echo "========================"
echo
