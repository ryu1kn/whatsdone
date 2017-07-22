#!/bin/bash

set -euo pipefail

if [ "$TRAVIS_EVENT_TYPE" = "cron" ] ; then
    BUILD_NUMBER=$TRAVIS_BUILD_NUMBER ENV_NAME=sit ./deploy-system.sh
else
    BUILD_NUMBER=$TRAVIS_BUILD_NUMBER ENV_NAME=prod ./deploy-system.sh
fi