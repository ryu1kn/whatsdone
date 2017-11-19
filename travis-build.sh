#!/bin/bash

set -euo pipefail

function main() {
    if [ "$TRAVIS_EVENT_TYPE" = "cron" ] ; then
        ENV_NAME=ci deploy_system
    elif [ "$TRAVIS_BRANCH" = "master" ] ; then
        ENV_NAME=prod build_and_deploy
    else
        ENV_NAME=dev-ryuichi build_and_deploy
    fi
}

function build_and_deploy() {
    echo Commit range: $TRAVIS_COMMIT_RANGE

    echo "List of changed file(s) in the range:"
    git diff --name-only $TRAVIS_COMMIT_RANGE
    echo

    git diff --name-only $TRAVIS_COMMIT_RANGE | BUILD_NUMBER=$TRAVIS_BUILD_NUMBER ./node_modules/.bin/buildmate
}

function deploy_system() {
    BUILD_NUMBER=$TRAVIS_BUILD_NUMBER ./deploy-system.sh
}

main "$@"
