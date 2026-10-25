
CI_USER_STACK := whatsdone-ci-user
CI_USER_NAME := travis-ci
CI_USER_PARAMETERS_FILE ?=

.PHONY: deploy-ci-user
deploy-ci-user:
	@test -r "$(CI_USER_PARAMETERS_FILE)" || { echo "Set CI_USER_PARAMETERS_FILE to a readable CloudFormation parameter JSON file." >&2; exit 1; }
	aws cloudformation deploy --stack-name $(CI_USER_STACK) --template-file ci-user.yaml \
		--capabilities CAPABILITY_NAMED_IAM \
		--no-fail-on-empty-changeset \
		--parameter-overrides "file://$(CI_USER_PARAMETERS_FILE)"

.PHONY: create-access-key
create-access-key:
	aws iam create-access-key --user-name $(CI_USER_NAME)
