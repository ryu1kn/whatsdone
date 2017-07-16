#!/bin/bash

set -euo pipefail

if [ "$TRAVIS_EVENT_TYPE" = "cron" ] ; then
    ENV_NAME=sit npm run deploy-system
else
    npm run build
fi
