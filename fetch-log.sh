#!/bin/bash

set -euo pipefail

LOG_GROUP=/aws/lambda/prod-whatsdone-GetDonesFunction
LOG_STREAM=`aws logs describe-log-streams --log-group-name $LOG_GROUP \
    | jq '.logStreams | map(.logStreamName) | sort | reverse | .[0]' --raw-output`
LOG_ENTRY_LIMIT=${1:-20}

aws logs get-log-events \
    --limit $LOG_ENTRY_LIMIT \
    --log-group-name $LOG_GROUP \
    --log-stream-name "$LOG_STREAM" | \
        jq '.events | map([(.timestamp / 1000 | todateiso8601), .message[:-1]] | join(" "))[]' --raw-output
