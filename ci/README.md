# Continuous Integration / Continuous Delivery

GitHub Actions uses OIDC roles to manage the app's cloud resources.

## Static analysis

Run `mise run gate`. Pull requests run the same checks.

## GitHub OIDC

Inspect existing providers, roles and stack ownership before setup. Reuse an existing account provider
only after confirming its owner. These templates create one retained account provider and separate
CI/Production roles. Resource-specific Checkov exceptions cover service provisioning and role management.

For initial setup, use an explicitly authorized profile with the required IAM and CloudFormation
permissions:

```sh
AWS_PROFILE=<authorized-profile> make deploy-github-oidc-provider
AWS_PROFILE=<authorized-profile> make deploy-github-oidc-roles
AWS_PROFILE=<authorized-profile> make show-github-oidc-role-arns
```

Restrict both CI and Production deployments to `main` in GitHub's environment settings before enabling
OIDC. AWS trusts the repository and environment; these settings enforce which branches can deploy.

> [!IMPORTANT] Actions stores temporary OIDC credentials in the `ci-whatsdone` profile. Kumo still uses
> AWS SDK for JavaScript v2, which cannot resolve some profile authentication methods. `deploy-module.sh`
> exports the profile's credentials to environment variables for Kumo.

References: [GitHub environment trust](https://docs.github.com/en/actions/how-tos/secure-your-work/security-harden-deployments/oidc-in-aws),
[optional provider thumbprints](https://docs.aws.amazon.com/AWSCloudFormation/latest/TemplateReference/aws-resource-iam-oidcprovider.html).

## Validate selected artifacts

To test a previously published build, open `build-and-deploy` in GitHub Actions,
select **Run workflow** on `main`, and enter the build identifier as `artifact_build`.
The workflow deploys that build to CI only, without rebuilding it or deploying to Production.
