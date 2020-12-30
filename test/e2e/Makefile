
node_modules:
	yarn install

.PHONY: test
test: node_modules
	$(MAKE) get-foo
	PASSWORD="$$($(MAKE) get-password)" node --unhandled-rejections=strict test

.PHONY: deploy
deploy:
	aws cloudformation deploy --stack-name whatsdone-e2e-test \
		--template-file resources.yaml \
		--no-fail-on-empty-changeset

.PHONY: get-password
get-password:
	@aws secretsmanager get-secret-value --secret-id /whatsdone/ci/e2e-test --region "$$AWS_REGION" \
		| jq -r '.SecretString | fromjson | .password'

.PHONY: get-foo
get-foo:
	aws secretsmanager get-secret-value --secret-id /whatsdone/ci/foo --region "$$AWS_REGION"
