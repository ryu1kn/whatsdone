#!/bin/bash

set -euo pipefail

function main() {
    if [[ "${1:-}" == "--deploy-selected-artifacts" ]]; then
        if [[ "${ENV_NAME:-}" != ci || "${BUILD_BRANCH:-}" != main ||
              ! "${BUILD_NUMBER:-}" =~ ^[a-zA-Z0-9][a-zA-Z0-9._-]*$ || "${BUILD_NUMBER}" == 0 ]]; then
            echo 'Selected deployment requires CI, main and a valid BUILD_NUMBER.' >&2
            exit 1
        fi
        install_build_dependencies
        install_old_glob
        ./deploy-module.sh --env ci backend api
        (cd modules/web-assets && yarn install && yarn run deploy:app)
        return
    fi
    if [[ "${BUILD_BRANCH:-}" != "main" ]] ; then
        echo 'Deployment can happen only from "main" branch'
        exit 0
    fi

    install_build_dependencies

    install_old_glob

    build_and_deploy
}

function build_and_deploy() {
    if grep -F '[CI:FULL_DEPLOY]' <<< "$COMMIT_MESSAGE" > /dev/null ; then
        # XXX: Some changes such as tools/ aren't tested with this. See buildmate.config.js.
        echo 'FULL_DEPLOY specified.'
        ./deploy-system.sh
    else
        local commit_range="$(get_commit_range)"
        echo "Commit range: $commit_range"

        echo 'List of changed file(s) in the range:'
        git diff --name-only "$commit_range"
        echo

        git diff --name-only "$commit_range" | ./node_modules/.bin/buildmate
    fi
}

function get_commit_range() {
    local one_line_message="$(tr '\n' ' ' <<< "$COMMIT_MESSAGE")"
    local potential_range="$(sed 's/.*\[COMMIT_RANGE:\([^]]*\)\].*/\1/' <<< "$one_line_message")"

    if  [[ "$potential_range" != "$one_line_message" ]] ; then
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

# glob-promise needs the callback API; kumo install resets package.json.
function install_old_glob() {
    (
        cd .kumo
        node -e "const fs=require('fs'); const p=require('./package.json'); p.resolutions={...p.resolutions, glob:'7.2.3'}; fs.writeFileSync('package.json', JSON.stringify(p, null, 2)+'\n')"
        yarn install
    )
}

main "$@"
