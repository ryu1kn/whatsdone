#!/bin/bash

set -euo pipefail

function main() {
    if [[ "${TRAVIS_BRANCH:-}" != "master" ]] ; then
        echo 'Deployment can happen only from "master" branch'
        exit 0
    fi

    install_old_glob

    build_and_deploy
}

function build_and_deploy() {
    local commit_range="$(get_commit_range)"
    echo "Commit range: $commit_range"

    echo 'List of changed file(s) in the range:'
    git diff --name-only "$commit_range"
    echo

    git diff --name-only "$commit_range" | BUILD_NUMBER="$TRAVIS_BUILD_NUMBER" ./node_modules/.bin/buildmate
}

function get_commit_range() {
    local one_line_message="$(tr '\n' ' ' <<< "$COMMIT_MESSAGE")"
    local potential_range="$(sed 's/.*\[COMMIT_RANGE:\([^]]*\)\].*/\1/' <<< "$one_line_message")"

    if [[ "$potential_range" != "$one_line_message" ]] ; then
        echo "$potential_range"
    else
        echo "$TRAVIS_COMMIT_RANGE"
    fi
}

# HACK: Manually install as glob-promise doesn't lock versions...
function install_old_glob() {
    (
        cd .kumo
        rm -rf node_modules/glob
        npm install glob@8.1.0
    )
}

main "$@"
