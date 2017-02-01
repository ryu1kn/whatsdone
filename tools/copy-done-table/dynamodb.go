package main

import "github.com/aws/aws-sdk-go/service/dynamodb"

type dynamodbWriter interface {
	BatchWriteItem(*dynamodb.BatchWriteItemInput) (*dynamodb.BatchWriteItemOutput, error)
}
