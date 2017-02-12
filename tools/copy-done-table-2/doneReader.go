package main

import (
	"github.com/aws/aws-sdk-go/aws"
	"github.com/aws/aws-sdk-go/service/dynamodb"
)

type _DoneReader struct {
	tableName string
	scanner   _IScanner
}

func (r *_DoneReader) readAll() (*_ReadAllResult, error) {
	scanOutput, err := r.scanner.scan(&dynamodb.ScanInput{TableName: aws.String(r.tableName)})
	return &_ReadAllResult{scanOutput.Items}, err
}

type _ReadAllResult struct {
	items []map[string]*dynamodb.AttributeValue
}
