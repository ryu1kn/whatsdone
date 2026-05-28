
# Continuous Integration / Continuous Delivery

What's Done works on AWS; hence you need an IAM user for CI/CD service to deploy the system.

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

To set the access key id and secret access key, update GitHub Actions secrets
[here](https://github.com/ryu1kn/whatsdone/settings/secrets/actions).

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
