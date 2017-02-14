package main

import "github.com/aws/aws-sdk-go/service/dynamodb"

type _IDynamoDBBatchWriteClient interface {
	BatchWriteItem(*dynamodb.BatchWriteItemInput) (*dynamodb.BatchWriteItemOutput, error)
}

type _BatchWriter struct {
	dynamoDB _IDynamoDBBatchWriteClient
}

func (bw *_BatchWriter) BatchWriteItem(input *dynamodb.BatchWriteItemInput) (*dynamodb.BatchWriteItemOutput, error) {
	return bw.dynamoDB.BatchWriteItem(input)
}
