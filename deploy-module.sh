# #!/bin/bash

set -euo pipefail

THIS_SCRIPT_NAME=$0
ENV_OPT=""

OPTIND=1    # Reset in case getopts has been used previously in the shell
while getopts "h?m:" opt; do
    case "$opt" in
    h|\?)
        echo $THIS_SCRIPT_NAME -m MODULE_NAME -e ENV_NAME -- ARGS_FOR_MODULE_DEPLOY
        exit 0
        ;;
    m)  MODULE_NAME=$OPTARG
        ;;
    esac
done
shift $((OPTIND-1))
[ "${1:- }" = "--" ] && shift

REST_ARGS=$@

cd modules/$MODULE_NAME
npm run deploy -- $REST_ARGS
