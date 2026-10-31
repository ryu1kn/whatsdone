OIDC_PROVIDER_STACK := whatsdone-github-oidc-provider
OIDC_ROLES_STACK := whatsdone-github-oidc-roles
GITHUB_ORG_REPO := ryu1kn/whatsdone

.PHONY: deploy-github-oidc
deploy-github-oidc: deploy-github-oidc-provider deploy-github-oidc-roles

.PHONY: deploy-github-oidc-provider
deploy-github-oidc-provider:
	aws cloudformation deploy \
		--stack-name $(OIDC_PROVIDER_STACK) \
		--template-file github-oidc-provider.yaml \
		--no-fail-on-empty-changeset

.PHONY: deploy-github-oidc-roles
deploy-github-oidc-roles:
	@test -n "$$(aws cloudformation describe-stacks --stack-name $(OIDC_PROVIDER_STACK) --query 'Stacks[0].StackName' --output text)" || \
		(echo "Deploy the provider stack first: make deploy-github-oidc-provider" && exit 1)
	aws cloudformation deploy \
		--stack-name $(OIDC_ROLES_STACK) \
		--template-file github-oidc-roles.yaml \
		--capabilities CAPABILITY_NAMED_IAM \
		--no-fail-on-empty-changeset \
		--parameter-overrides \
			"GitHubOidcProviderArn=$$(aws cloudformation describe-stacks --stack-name $(OIDC_PROVIDER_STACK) --query 'Stacks[0].Outputs[?OutputKey==`OidcProviderArn`].OutputValue' --output text)" \
			"GitHubOrgRepo=$(GITHUB_ORG_REPO)"

.PHONY: show-github-oidc-role-arns
show-github-oidc-role-arns:
	@aws cloudformation describe-stacks --stack-name $(OIDC_ROLES_STACK) \
		--query 'Stacks[0].Outputs' --output table
