package main

import "github.com/aws/aws-sdk-go/service/dynamodb"

type _DoneWriter struct {
	tableName           string
	dynamoDBBatchWriter _IBatchWriter
}

func (dw *_DoneWriter) write(items *[]map[string]*dynamodb.AttributeValue) error {
	writeRequests := make([]*dynamodb.WriteRequest, len(*items))
	for i, item := range *items {
		writeRequests[i] = &dynamodb.WriteRequest{
			PutRequest: &dynamodb.PutRequest{Item: item},
		}
	}
	writeInput := dynamodb.BatchWriteItemInput{
		RequestItems: map[string][]*dynamodb.WriteRequest{
			dw.tableName: writeRequests,
		},
	}
	_, err := dw.dynamoDBBatchWriter.BatchWriteItem(&writeInput)
	return err
}
