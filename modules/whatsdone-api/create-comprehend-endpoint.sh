#!/usr/bin/env bash

set -euo pipefail

readonly account_id="$(aws sts get-caller-identity --query Account --output text)"
readonly endpoint_name="whatsdone-$ENV-topic-classifier"
readonly model_arn="arn:aws:comprehend:$TASK_REGION:$account_id:document-classifier/whatsdone-topic-classifier"

readonly endpoint_arn="arn:aws:comprehend:$TASK_REGION:$account_id:document-classifier-endpoint/$endpoint_name"

readonly endpoints="$(aws comprehend list-endpoints | jq --raw-output '.EndpointPropertiesList | map(.EndpointArn)')[]"

if grep -F "$endpoint_arn" <<< "$endpoints" ; then
    aws comprehend update-endpoint --endpoint-arn "$endpoint_arn" \
        --desired-model-arn "$model_arn" \
        --desired-inference-units 3
    echo "\"$endpoint_arn\"" > "$TASK_OUTPUTS_FILE"
else
    aws comprehend create-endpoint --endpoint-name "$endpoint_name" \
        --model-arn "$model_arn" \
        --desired-inference-units 3 \
            | jq .EndpointArn > "$TASK_OUTPUTS_FILE"
fi
