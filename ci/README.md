# Continuous Integration / Continuous Delivery

GitHub Actions uses OIDC roles and requires no IAM user or access keys.
The legacy CI user is pending separately approved retirement.

## Static analysis

`mise run gate` runs cfn-lint, Checkov and module tests. Pull requests run the same checks.
Run `mise run cfn-lint` or `mise run checkov` separately for CI templates.
Configuration: `.cfnlintrc.yaml` and `.checkov.yml`.

## GitHub OIDC

Inspect existing providers, roles and stack ownership before setup. Reuse an
existing account provider only after confirming its owner. These templates create
one retained account provider and separate CI/Production roles. Role permissions
match deployment needs and remain broad; resource-specific Checkov exceptions
cover service provisioning and role management. Cognito resources and the E2E secret stack are independent and must be preserved.

With approval and provider-creation privileges:

```sh
AWS_PROFILE=ci-whatsdone make deploy-github-oidc-provider
AWS_PROFILE=ci-whatsdone make deploy-github-oidc-roles
AWS_PROFILE=ci-whatsdone make show-github-oidc-role-arns
```

Set each GitHub environment's `AWS_ROLE_ARN` secret to its corresponding role.
Restrict CI deployments to `main` only;
restrict Production to `main`. Environment subjects contain no branch claim, so
GitHub environment branch policies enforce permitted refs. Do not enable OIDC
until those restrictions are configured. No access keys are required by Actions.
Actions writes assumed credentials to the `ci-whatsdone` profile. Profile-only
output avoids credential lookup before that profile exists; deployment exports
its credentials for the older deployment SDK.

References: [GitHub environment trust](https://docs.github.com/en/actions/how-tos/secure-your-work/security-harden-deployments/oidc-in-aws),
[optional provider thumbprints](https://docs.aws.amazon.com/AWSCloudFormation/latest/TemplateReference/aws-resource-iam-oidcprovider.html).

## Validate selected artifacts

Dispatch `build-and-deploy` from `main` with the previously published
`artifact_build`. Only `ci-validation` runs. Production jobs require a push to
`main`. Manual dispatch requires the workflow to exist on the default branch.
The command promotes backend/web artifacts without rebuilding.
`BUILD_NUMBER` is required on publishing/deployment steps; mise preserves it.

Capture CI resource and record evidence before and after deployment. Compare
selected ZIPs with deployed bytes and confirm DONEs load. Hosted role assumption
is required to verify OIDC; local checks cannot establish it.
