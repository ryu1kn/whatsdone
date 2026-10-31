
# Continuous Integration / Continuous Delivery

GitHub Actions uses OIDC roles. Keep the existing CI user for laptop operations.

Creation of the user is done only at the beginning; so I just provide the CloudFormation template and Makefile for this.
After you create a user, you need to create an access key.

## Create an IAM user

To create/update the user:

```sh
$ AWS_PROFILE=<YOUR_PROFILE_NAME> make deploy-ci-user CI_USER_PARAMETERS_FILE=/path/to/parameters.json
```

Use a CloudFormation `ParameterKey`/`ParameterValue` JSON file containing `CiUserName`,
`ReadOnlyUserPoolArn`, `ReadOnlyCiUserPoolArn`, and `ReadOnlyIdentityPoolArns` (comma-separated ARNs).
Keep `CiUserName` unchanged when updating an existing user.

To create an access key for the user

```sh
$ AWS_PROFILE=<YOUR_PROFILE_NAME> make create-access-key
```

Make sure you take a memo of `SecretAccessKey` in the output. You can only see this at this timing.

## Static analysis

`mise run gate` runs cfn-lint, Checkov and module tests. Pull requests run the same checks.
Run `mise run cfn-lint` or `mise run checkov` separately for CI templates.
Configuration: `.cfnlintrc.yaml` and `.checkov.yml`.

The existing CI deployment group requires broad resource access for CloudFormation,
service provisioning and IAM role creation. Checkov suppressions on that group
cover wildcard reads (CKV_AWS_108), permissions management (CKV_AWS_109),
privilege escalation (CKV_AWS_110) and resource-wide writes (CKV_AWS_111).
These permissions remain a security risk; tightening deployment access requires
a separate permissions review and approved stack update. Other resources remain checked.

cfn-lint W3037 is suppressed only on the deployment group because its legacy
`apigateway:HEAD` and `apigateway:OPTIONS` actions are unrecognized by IAM.
The policy is preserved pending the same permissions review.

## GitHub OIDC

Inspect existing providers, roles and stack ownership before setup. Reuse an
existing account provider only after confirming its owner. These templates create
one retained account provider and separate CI/Production roles. Role permissions
match deployment needs and remain broad; resource-specific Checkov exceptions
cover service provisioning and role management. Existing CI-user permissions,
Cognito resources and the E2E secret stack are unchanged.

With approval and provider-creation privileges:

```sh
AWS_PROFILE=ci-whatsdone make deploy-github-oidc-provider
AWS_PROFILE=ci-whatsdone make deploy-github-oidc-roles
AWS_PROFILE=ci-whatsdone make show-github-oidc-role-arns
```

Set each GitHub environment's `AWS_ROLE_ARN` secret to its corresponding role.
Restrict CI deployments to `main` and `main-new` only;
restrict Production to `main`. Environment subjects contain no branch claim, so
GitHub environment branch policies enforce permitted refs. Do not enable OIDC
until those restrictions are configured. No access keys are required by Actions.
Actions writes assumed credentials to the `ci-whatsdone` profile. Profile-only
output avoids credential lookup before that profile exists; deployment exports
its credentials for the older deployment SDK.

References: [GitHub environment trust](https://docs.github.com/en/actions/how-tos/secure-your-work/security-harden-deployments/oidc-in-aws),
[optional provider thumbprints](https://docs.aws.amazon.com/AWSCloudFormation/latest/TemplateReference/aws-resource-iam-oidcprovider.html).

## Validate selected artifacts

After reviewing CI resource changes, authorize the branch push and hosted run
separately. Before merging, set CI's `CI_ARTIFACT_BUILD` variable to the selected
published build, then push `main-new` with approval. That push runs only
`ci-validation`; production jobs require a push to `main`.

Manual dispatch requires this workflow to exist on the default branch first.
Once available, dispatch `build-and-deploy` from `main-new` (or `main`) with
`artifact_build`. Both paths promote backend/web artifacts without rebuilding.
`BUILD_NUMBER` is set on each publishing/deployment step because mise initializes
it to `0`.

Capture CI resource and record evidence before and after deployment. Compare
selected ZIPs with deployed bytes and confirm DONEs load. Hosted role assumption
is required to verify OIDC; local checks cannot establish it.
