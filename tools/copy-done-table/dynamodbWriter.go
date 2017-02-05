package main

import "github.com/aws/aws-sdk-go/service/dynamodb"

type _IDynamoDBBatchWriter interface {
	BatchWriteItem(*dynamodb.BatchWriteItemInput) error
}

type _DynamoDBBatchWriter struct {
	client dynamodb.DynamoDB
}

func (d *_DynamoDBBatchWriter) BatchWriteItem(input *dynamodb.BatchWriteItemInput) error {
	_, error := d.client.BatchWriteItem(input)
	return error
}
