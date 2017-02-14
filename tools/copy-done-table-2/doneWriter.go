package main

import "github.com/aws/aws-sdk-go/service/dynamodb"

const _MaxBatchWriteCount = 25

type _DoneWriter struct {
	tableName           string
	dynamoDBBatchWriter _IBatchWriter
}

func (dw *_DoneWriter) write(items *[]map[string]*dynamodb.AttributeValue) error {
	numOfItems := len(*items)
	for i := 0; i < numOfItems; i += _MaxBatchWriteCount {
		var subList []map[string]*dynamodb.AttributeValue
		if i+_MaxBatchWriteCount < numOfItems {
			subList = (*items)[i : i+_MaxBatchWriteCount]
		} else {
			subList = (*items)[i:]
		}
		err := dw._write(&subList)
		if err != nil {
			return err
		}
	}
	return nil
}

func (dw *_DoneWriter) _write(items *[]map[string]*dynamodb.AttributeValue) error {
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
