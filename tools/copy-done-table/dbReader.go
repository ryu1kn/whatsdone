package main

import "github.com/aws/aws-sdk-go/service/dynamodb"

type _IDoneReader interface {
	read() (*_IDynamoDBScanOutput, error)
}

type _IDynamoDBScanner interface {
	Scan(*dynamodb.ScanInput) (*_IDynamoDBScanOutput, error)
}

type _IDynamoDBScanOutput interface {
	Items() *[]_IDoneItem
}

type _IDoneItem interface{}

type _DoneReader struct {
	scanner   _IDynamoDBScanner
	tableName string
}

type _DynamoDBScanner struct {
	raw *dynamodb.ScanOutput
}

type _DoneItem map[string]*dynamodb.AttributeValue

func (output *_DynamoDBScanner) Items() *[]_IDoneItem {
	doneItems := make([]_IDoneItem, len(output.raw.Items))
	for i, doneItem := range output.raw.Items {
		doneItems[i] = doneItem
	}
	return &doneItems
}

func (r _DoneReader) read() (*_IDynamoDBScanOutput, error) {
	return r.scanner.Scan(&dynamodb.ScanInput{TableName: &r.tableName})
}
