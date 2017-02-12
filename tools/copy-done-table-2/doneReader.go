package main

import (
	"github.com/aws/aws-sdk-go/aws"
	"github.com/aws/aws-sdk-go/service/dynamodb"
)

type _DoneReader struct {
	tableName string
	scanner   _IScanner
}

func (r *_DoneReader) readAll() (*dynamodb.ScanOutput, error) {
	return r.scanner.scan(&dynamodb.ScanInput{TableName: aws.String(r.tableName)})
}

type _ReadAllResult struct {
	_items *[]_DoneItem
}

func (r *_ReadAllResult) count() int {
	return len(*r.items())
}

func (r *_ReadAllResult) items() *[]_DoneItem {
	return r._items
}

type _DoneItem struct {
	value string
}
