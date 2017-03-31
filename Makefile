# This file is for Dev environment

ENV_VARS := $(shell paste -d' ' -s __local-env)

start:
	$(ENV_VARS) npm start

quick-start:
	$(ENV_VARS) node ./bin/www
