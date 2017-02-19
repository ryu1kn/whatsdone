package main

import (
	"github.com/aws/aws-sdk-go/service/dynamodb"
)

type _IDynamoDBScannerClient interface {
	Scan(*dynamodb.ScanInput) (*dynamodb.ScanOutput, error)
}

type _IDynamoDBScanner interface {
	scan(*dynamodb.ScanInput) (*dynamodb.ScanOutput, error)
}

type _DynamoDBScanner struct {
	dynamoDB _IDynamoDBScannerClient
}

func (s *_DynamoDBScanner) scan(input *dynamodb.ScanInput) (*dynamodb.ScanOutput, error) {
	return s.dynamoDB.Scan(input)
}
