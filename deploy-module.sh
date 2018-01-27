#!/bin/bash

set -euo pipefail

ENV_NAME=""
POSITIONAL_ARGS=($0)

main() {
    parseArgs "$@"

    for MODULE in ${POSITIONAL_ARGS[@]:1} ; do
        echo === Deploying module $MODULE ===
        (cd modules/$MODULE && yarn run deploy --env $ENV_NAME --region $AWS_REGION)
        echo
    done
}

parseArgs() {
    while [[ $# -gt 0 ]] ; do
        case "$1" in
            -h|--help)
                printUsage
                exit 0
                ;;

            -e|--env)
                ENV_NAME=$2
                shift; shift
                ;;

            *)
                POSITIONAL_ARGS+=("$1")
                shift
                ;;
        esac
    done
}

printUsage() {
    cat << EOF

  Usage: $0 [-e env_name] module_name

  Options:

    -h, --help              Print this information
    -e, --env <env_name>    Environment name
    module_name             Name of module to deploy
EOF
}

main "$@"
