#!/usr/bin/env bash

set -euo pipefail

readonly userPoolId="$(aws cognito-idp describe-user-pool-domain --domain "$USER_POOL_DOMAIN" --region "$TASK_REGION" \
                | jq --raw-output .DomainDescription.UserPoolId)"

if [[ "$userPoolId" = null ]] ; then
    aws cognito-idp create-user-pool-domain --user-pool-id "$USER_POOL_ID" --domain "$USER_POOL_DOMAIN" --region "$TASK_REGION"
elif [[ "$userPoolId" = "$USER_POOL_ID" ]] ; then
    echo '...User pool domain is already created for the specified user pool'
else
    echo '...User pool domain is already taken by another user pool'
    exit 1
fi
