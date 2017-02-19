package main

import "github.com/aws/aws-sdk-go/service/dynamodb"

type _DoneCollection struct {
	items []map[string]*dynamodb.AttributeValue
}
