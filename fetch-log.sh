#!/bin/bash

set -euo pipefail

log_group=/aws/lambda/whatsdone-prod-ProxyLambdaFunction
log_stream=`aws logs describe-log-streams --log-group-name $log_group \
    | jq '.logStreams | map(.logStreamName) | sort | reverse | .[0]' --raw-output`
log_entry_limit=${1:-20}

aws logs get-log-events \
    --limit $log_entry_limit \
    --log-group-name $log_group \
    --log-stream-name "$log_stream" | \
        jq '.events | map([(.timestamp / 1000 | todateiso8601), .message[:-1]] | join(" "))[]' --raw-output
