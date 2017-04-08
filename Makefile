# NOTE: AWS_PROFILE must be set
ENV_NAME := prod
REGION := ap-southeast-2

MOD_API_DIR := modules/whatsdone-api
MOD_APP_DIR := modules/whatsdone-app

deploy-api:
	(cd $(MOD_API_DIR) && kumo deploy-module --region $(REGION) --verbose --env $(ENV_NAME))

destroy-api:
	(cd $(MOD_API_DIR) && kumo destroy-module --region $(REGION) --verbose --env $(ENV_NAME))

deploy-app:
	(cd $(MOD_APP_DIR) && kumo deploy-module --region $(REGION) --verbose --env $(ENV_NAME))

destroy-app:
	(cd $(MOD_APP_DIR) && kumo destroy-module --region $(REGION) --verbose --env $(ENV_NAME))

test-app:
	(cd $(MOD_APP_DIR)/src && npm test)
