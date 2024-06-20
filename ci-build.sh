#!/bin/bash

set -euo pipefail

function main() {
    if [[ "${BUILD_BRANCH:-}" != "main" ]] ; then
        echo 'Deployment can happen only from "main" branch'
        exit 0
    fi

    install_build_dependencies

    install_old_glob

    build_and_deploy
}

function build_and_deploy() {
    local commit_range="$(get_commit_range)"
    echo "Commit range: $commit_range"

    echo 'List of changed file(s) in the range:'
    git diff --name-only "$commit_range"
    echo

    git diff --name-only "$commit_range" | ./node_modules/.bin/buildmate
}

function get_commit_range() {
    local one_line_message="$(tr '\n' ' ' <<< "$COMMIT_MESSAGE")"
    local potential_range="$(sed 's/.*\[COMMIT_RANGE:\([^]]*\)\].*/\1/' <<< "$one_line_message")"

    if [[ "$potential_range" != "$one_line_message" ]] ; then
        echo "$potential_range"
    else
        echo "$BUILD_COMMIT_RANGE"
    fi
}

function install_build_dependencies() {
    aws configure set s3.signature_version s3v4
    yarn global add kumo
    kumo install
    yarn install
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
