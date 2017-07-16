#!/bin/bash

set -euo pipefail

if [ "$TRAVIS_EVENT_TYPE" = "cron" ] ; then
    CI_BUILD_NUMBER=$TRAVIS_BUILD_NUMBER ENV_NAME=sit npm run deploy-system
else
    CI_BUILD_NUMBER=$TRAVIS_BUILD_NUMBER npm run build
fi
