
.PHONY: test
test:
	E2E_USER_PASSWORD="$$($(MAKE) --no-print-directory get-password)" node --unhandled-rejections=strict test

.PHONY: deploy
deploy:
	aws cloudformation deploy --stack-name whatsdone-e2e-test \
		--template-file resources.yaml \
		--no-fail-on-empty-changeset

.PHONY: get-password
get-password:
	@aws secretsmanager get-secret-value --secret-id /whatsdone/ci/e2e-test --region "$$AWS_REGION" \
		| jq -r '.SecretString | fromjson | .password'
