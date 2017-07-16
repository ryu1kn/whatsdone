
CI_USER_STACK := whatsdone-ci-user
CI_USER_NAME := travis-ci

create-ci-user:
	aws cloudformation create-stack --stack-name ${CI_USER_STACK} --template-body="`cat ci-user.yaml`" \
		--capabilities CAPABILITY_NAMED_IAM \
		--parameters \
			ParameterKey=CiUserName,ParameterValue=$(CI_USER_NAME),UsePreviousValue=false

update-ci-user:
	aws cloudformation update-stack --stack-name ${CI_USER_STACK} --template-body="`cat ci-user.yaml`" \
		--capabilities CAPABILITY_NAMED_IAM \
		--parameters \
			ParameterKey=CiUserName,ParameterValue=$(CI_USER_NAME),UsePreviousValue=false

create-access-key:
	aws iam create-access-key --user-name $(CI_USER_NAME)
