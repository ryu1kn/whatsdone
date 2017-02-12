package main

import (
	"github.com/aws/aws-sdk-go/service/dynamodb"
)

type _IDynamoDB interface {
	Scan(*dynamodb.ScanInput) (*dynamodb.ScanOutput, error)
}

type _Scanner struct {
	dynamoDB _IDynamoDB
}

func (s *_Scanner) scan(input *dynamodb.ScanInput) (*dynamodb.ScanOutput, error) {
	return s.dynamoDB.Scan(input)
}
