
node_modules:
	yarn install

.PHONY: test
test: node_modules
	PASSWORD="$$($(MAKE) get-password)" node --unhandled-rejections=strict test

.PHONY: deploy
deploy:
	aws cloudformation deploy --stack-name whatsdone-e2e-test \
		--template-file resources.yaml \
		--no-fail-on-empty-changeset

.PHONY: get-password
get-password:
	@aws secretsmanager get-secret-value --secret-id /whatsdone/ci/e2e-test \
		| jq -r '.SecretString | fromjson | .password'
