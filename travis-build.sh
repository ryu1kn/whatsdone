#!/bin/bash

set -euo pipefail

function main() {
    if [[ "${TRAVIS_BRANCH:-}" != "master" ]] ; then
        echo 'Deployment can happen only from "master" branch'
        exit 0
    fi

    build_and_deploy
}

function build_and_deploy() {
    echo "Commit range: $TRAVIS_COMMIT_RANGE"

    echo 'List of changed file(s) in the range:'
    git diff --name-only "$TRAVIS_COMMIT_RANGE"
    echo

    git diff --name-only "$TRAVIS_COMMIT_RANGE" | BUILD_NUMBER="$TRAVIS_BUILD_NUMBER" ./node_modules/.bin/buildmate
}

main "$@"
