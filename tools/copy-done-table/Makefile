
# Set TABLE_NAME as an environment variable to execute targets

dumpFile = __$(TABLE_NAME).json

migrate: build
	./__main --from-table-name whatsdone-dones --from-table-region ap-northeast-1 --to-table-name $(TABLE_NAME)

build:
	go build -o __main

dump-as-json: delete-dump-file
	aws dynamodb scan --table-name $(TABLE_NAME) --region $(AWS_REGION) | jq -f __filter.txt >| $(dumpFile) && jq length $(dumpFile)

describe-table:
	aws dynamodb describe-table --table-name $(TABLE_NAME) --region $(AWS_REGION)

recreate-table: delete-table create-table

create-table:
	aws dynamodb create-table --region $(AWS_REGION) \
		--table-name $(TABLE_NAME) \
		--key-schema AttributeName=id,KeyType=HASH \
		--attribute-definitions AttributeName=id,AttributeType=S \
		--provisioned-throughput ReadCapacityUnits=1,WriteCapacityUnits=1

delete-table:
	aws dynamodb delete-table --table-name $(TABLE_NAME) --region $(AWS_REGION)

delete-dump-file:
	rm -f $(dumpFile)

test:
	go test

clean:
	rm -f __main
