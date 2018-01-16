
CI_USER_STACK := whatsdone-ci-user
CI_USER_NAME := travis-ci

create-ci-user:
	aws cloudformation create-stack --stack-name ${CI_USER_STACK} --template-body="`cat ci-user.yaml`" \
		--capabilities CAPABILITY_NAMED_IAM \
		--parameters \
			ParameterKey=CiUserName,ParameterValue=$(CI_USER_NAME),UsePreviousValue=false
	aws cloudformation wait stack-create-complete --stack-name ${CI_USER_STACK}

update-ci-user:
	aws cloudformation update-stack --stack-name ${CI_USER_STACK} --template-body="`cat ci-user.yaml`" \
		--capabilities CAPABILITY_NAMED_IAM \
		--parameters \
			ParameterKey=CiUserName,ParameterValue=$(CI_USER_NAME),UsePreviousValue=false
	aws cloudformation wait stack-update-complete --stack-name ${CI_USER_STACK}

create-access-key:
	aws iam create-access-key --user-name $(CI_USER_NAME)
