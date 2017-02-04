package main

import "github.com/aws/aws-sdk-go/service/dynamodb"

type _IDynamoDBWriter interface {
	BatchWriteItem(*dynamodb.BatchWriteItemInput) (*dynamodb.BatchWriteItemOutput, error)
}
