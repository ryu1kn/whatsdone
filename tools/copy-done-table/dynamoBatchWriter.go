package main

import "github.com/aws/aws-sdk-go/service/dynamodb"

type _IDynamoDBBatchWriteClient interface {
	BatchWriteItem(*dynamodb.BatchWriteItemInput) (*dynamodb.BatchWriteItemOutput, error)
}

type _IDynamoDBBatchWriter interface {
	BatchWriteItem(*dynamodb.BatchWriteItemInput) (*dynamodb.BatchWriteItemOutput, error)
}

type _DynamoDBBatchWriter struct {
	dynamoDB _IDynamoDBBatchWriteClient
}

func (bw *_DynamoDBBatchWriter) BatchWriteItem(input *dynamodb.BatchWriteItemInput) (*dynamodb.BatchWriteItemOutput, error) {
	return bw.dynamoDB.BatchWriteItem(input)
}
