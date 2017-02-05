package main

import "github.com/aws/aws-sdk-go/service/dynamodb"

type _IDynamoDBScanner interface {
	Scan(*dynamodb.ScanInput) (*_IDynamoDBScanOutput, error)
}

type _IDynamoDBScanOutput interface {
	Items() *[]_IDoneItem // XXX: Don't refer to app knowledge
}

type _DynamoDBScanner struct {
	client dynamodb.DynamoDB
}

func (d *_DynamoDBScanner) Scan(input *dynamodb.ScanInput) (*_IDynamoDBScanOutput, error) {
	scanOutput, error := d.client.Scan(input)
	var output _IDynamoDBScanOutput = &_DynamoDBScanOutput{scanOutput}
	return &output, error
}
