
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
`ReadOnlyUserPoolArn`, and `ReadOnlyIdentityPoolArns` (comma-separated ARNs).
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

Set each GitHub environment's `AWS_ROLE_ARN` variable to its corresponding role.
Restrict CI deployments to `main` and the explicitly approved validation branch;
restrict Production to `main`. Environment subjects contain no branch claim, so
GitHub environment branch policies enforce permitted refs. Do not enable OIDC
until those restrictions are configured. No access keys are required by Actions.

References: [GitHub environment trust](https://docs.github.com/en/actions/how-tos/secure-your-work/security-harden-deployments/oidc-in-aws),
[optional provider thumbprints](https://docs.aws.amazon.com/AWSCloudFormation/latest/TemplateReference/aws-resource-iam-oidcprovider.html).
