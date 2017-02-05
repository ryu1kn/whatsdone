package main

import "github.com/aws/aws-sdk-go/service/dynamodb"

type _IDoneReader interface {
	read() (*[]_IDoneItem, error)
}

type _DoneReader struct {
	scanner   _IDynamoDBScanner
	tableName string
}

func (r _DoneReader) read() (*[]_IDoneItem, error) {
	scanOutput, error := r.scanner.Scan(&dynamodb.ScanInput{TableName: &r.tableName})
	if error != nil {
		return nil, error
	}

	items := *(*scanOutput).Items()
	doneItems := make([]_IDoneItem, len(items))
	for i, item := range items {
		doneItems[i] = item
	}
	return &doneItems, nil
}
