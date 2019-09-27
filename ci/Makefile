
CI_USER_STACK := whatsdone-ci-user
CI_USER_NAME := travis-ci

.PHONY: deploy-ci-user
deploy-ci-user:
	aws cloudformation deploy --stack-name $(CI_USER_STACK) --template-file ci-user.yaml \
		--capabilities CAPABILITY_NAMED_IAM \
		--no-fail-on-empty-changeset \
		--parameter-overrides "CiUserName=$(CI_USER_NAME)"

.PHONY: create-access-key
create-access-key:
	aws iam create-access-key --user-name $(CI_USER_NAME)
