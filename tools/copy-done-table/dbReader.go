package main

import "github.com/aws/aws-sdk-go/service/dynamodb"

type DoneItem interface{}

type DoneItemImpl map[string]*dynamodb.AttributeValue

type DoneReader interface {
	read() (*dynamodbScanOutput, error)
}

type DoneReaderImpl struct {
	scanner   dynamodbScanner
	tableName string
}

type dynamodbScanner interface {
	Scan(*dynamodb.ScanInput) (*dynamodbScanOutput, error)
}

type dynamodbScanOutput interface {
	Items() *[]DoneItem
}

type dynamodbScanOutputImpl struct {
	raw *dynamodb.ScanOutput
}

func (output *dynamodbScanOutputImpl) Items() *[]DoneItem {
	doneItems := make([]DoneItem, len(output.raw.Items))
	for i, doneItem := range output.raw.Items {
		doneItems[i] = doneItem
	}
	return &doneItems
}

func (r DoneReaderImpl) read() (*dynamodbScanOutput, error) {
	return r.scanner.Scan(&dynamodb.ScanInput{TableName: &r.tableName})
}
