package main

import "github.com/aws/aws-sdk-go/service/dynamodb"

type _IDynamoDBScanner interface {
	Scan(*dynamodb.ScanInput) (*_IDynamoDBScanOutput, error)
}

type _IDynamoDBScanOutput interface {
	Items() *[]_IDynamoDBItem
}

type _IDynamoDBItem interface{}

type _DynamoDBScanner struct {
	client dynamodb.DynamoDB
}

func (d *_DynamoDBScanner) Scan(input *dynamodb.ScanInput) (*_IDynamoDBScanOutput, error) {
	scanOutput, error := d.client.Scan(input)
	var output _IDynamoDBScanOutput = &_DynamoDBScanOutput{scanOutput}
	return &output, error
}

type _DynamoDBScanOutput struct {
	raw *dynamodb.ScanOutput
}

func (output *_DynamoDBScanOutput) Items() *[]_IDynamoDBItem {
	doneItems := make([]_IDynamoDBItem, len(output.raw.Items))
	for i, doneItem := range output.raw.Items {
		doneItems[i] = doneItem
	}
	return &doneItems
}
