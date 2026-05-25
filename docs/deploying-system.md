# Deploying the system

The target account needs the hosted zone in `config/common.js` and an issued
certificate matching `certDomainName` in `us-east-1`. Preserve existing domains,
resource names, Cognito IDs, encryption and artifact/output locations.

Select an authorized AWS profile and environment explicitly:

```sh
AWS_PROFILE=your-profile AWS_REGION=ap-southeast-2 ENV_NAME=your-environment ./deploy-system.sh
```

For a new environment, read `UserPoolId` and `IdentityPoolId` from the
authentication deployment outputs. Find the `Webapp` client ID with
`aws cognito-idp list-user-pool-clients --user-pool-id "$USER_POOL_ID"`.
Put these IDs in `modules/web-assets/app-config/<environment>.json`, then
redeploy the web assets.
Keep existing environments' IDs unchanged.

## Provisioning a new user

Use the existing user pool and user-ID table for the selected environment.
Generate a temporary password locally; never commit it:

```sh
export AWS_PROFILE=your-profile AWS_REGION=ap-southeast-2
password="Aa1!$(openssl rand -hex 24)"
aws cognito-idp admin-create-user --user-pool-id "$USER_POOL_ID" \
  --username "$USERNAME" --message-action SUPPRESS --temporary-password "$password"
unset password
aws cognito-idp admin-get-user --user-pool-id "$USER_POOL_ID" \
  --username "$USERNAME" --query 'UserAttributes[?Name==`sub`].Value' --output text
```

Use that Cognito `sub` as `cognitoUserId` in a new user-ID table record.
Generate its `id` with `uuidgen`. Do not overwrite an existing mapping.
