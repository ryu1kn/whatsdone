package main

import "github.com/aws/aws-sdk-go/service/dynamodb"

type DoneItem interface {
}

type DoneItemImpl map[string]*dynamodb.AttributeValue

type DoneReader interface {
	read() (*[]DoneItem, error)
}

type DoneReaderImpl struct {
	scanner   dynamodbScanner
	tableName string
}

type dynamodbScanner interface {
	Scan(*dynamodb.ScanInput) (*dynamodb.ScanOutput, error)
}

func (r DoneReaderImpl) read() (*[]DoneItem, error) {
	scanOutput, err := r.scanner.Scan(&dynamodb.ScanInput{TableName: &r.tableName})
	if err != nil {
		return nil, err
	}
	doneItems := make([]DoneItem, len(scanOutput.Items))
	for i, doneItem := range scanOutput.Items {
		doneItems[i] = doneItem
	}
	return &doneItems, nil
}
