#!/bin/bash

set -euo pipefail

log_group=/aws/lambda/whatsdone-prod-ProxyLambdaFunction
log_entry_limit=20
usage_message=$(cat << EOF

  Description:  Query CloudWatch logs.

  Usage:        $0 [options...]

  Options:
    -l log_group        Log group name. Default: $log_group
    -e log_entry_limit  Log entry limit. Default: $log_entry_limit
    -h                  Help. Print this usage information.

EOF
)

while [ $# -gt 0 ] ; do
    case $1 in
        -l) log_group="$2"; shift ;;
        -e) log_entry_limit="$2"; shift ;;
        -h) echo "$usage_message" ; exit 0 ;;
         *) break
    esac
    shift
done

log_stream=`aws logs describe-log-streams --log-group-name $log_group \
    | jq '.logStreams | map(.logStreamName) | sort | reverse | .[0]' --raw-output`

aws logs get-log-events \
    --limit $log_entry_limit \
    --log-group-name $log_group \
    --log-stream-name "$log_stream" | \
        jq '.events | map([(.timestamp / 1000 | todateiso8601), .message[:-1]] | join(" "))[]' --raw-output
