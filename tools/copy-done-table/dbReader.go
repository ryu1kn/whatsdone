package main

import "github.com/aws/aws-sdk-go/service/dynamodb"

type _IDoneReader interface {
	read() (*_IDynamoDBScanOutput, error)
}

type _IDoneItem interface{}

type _DoneReader struct {
	scanner   _IDynamoDBScanner
	tableName string
}

func (r _DoneReader) read() (*_IDynamoDBScanOutput, error) {
	return r.scanner.Scan(&dynamodb.ScanInput{TableName: &r.tableName})
}

type _DynamoDBScanOutput struct {
	raw *dynamodb.ScanOutput
}

func (output *_DynamoDBScanOutput) Items() *[]_IDoneItem {
	doneItems := make([]_IDoneItem, len(output.raw.Items))
	for i, doneItem := range output.raw.Items {
		doneItems[i] = doneItem
	}
	return &doneItems
}

type _DoneItem map[string]*dynamodb.AttributeValue
