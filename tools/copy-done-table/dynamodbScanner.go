package main

import "github.com/aws/aws-sdk-go/service/dynamodb"

type _IDynamoDBScanner interface {
	Scan(*dynamodb.ScanInput) (*_IDynamoDBScanOutput, error)
}

type _IDynamoDBScanOutput interface {
	Items() *[]_IDoneItem // XXX: Don't refer to app knowledge
}
