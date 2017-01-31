package main

import "github.com/aws/aws-sdk-go/service/dynamodb"

type dynamodbScanner interface {
	Scan(*dynamodb.ScanInput) (*dynamodb.ScanOutput, error)
}

type dynamodbWriter interface {
	BatchWriteItem(*dynamodb.BatchWriteItemInput) (*dynamodb.BatchWriteItemOutput, error)
}
