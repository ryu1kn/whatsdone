package main

import "github.com/aws/aws-sdk-go/service/dynamodb"

const _MaxBatchWriteCount = 25

type _DoneWriter struct {
	tableName           string
	dynamoDBBatchWriter _IBatchWriter
}

// TODO: Receive [] instead of *[]
func (dw *_DoneWriter) write(items *[]map[string]*dynamodb.AttributeValue) error {
	numOfItems := len(*items)
	retryItems := make([]*dynamodb.WriteRequest, 0)
	var maxNumOfNewItems int

	for i, numOfRetryItems := 0, 0; i < numOfItems; i += maxNumOfNewItems {
		maxNumOfNewItems = _MaxBatchWriteCount - numOfRetryItems
		var newItems []map[string]*dynamodb.AttributeValue
		if i+maxNumOfNewItems < numOfItems {
			newItems = (*items)[i : i+maxNumOfNewItems]
		} else {
			newItems = (*items)[i:]
		}
		input := dw._buildBatchWriteItemInput(retryItems, newItems)
		output, err := dw.dynamoDBBatchWriter.BatchWriteItem(input)
		if err != nil {
			return err
		}

		retryItems = dw._extractUnprocessedRequests(output)
		numOfRetryItems = len(retryItems)
	}
	return nil
}

func (dw *_DoneWriter) _buildBatchWriteItemInput(retryItems []*dynamodb.WriteRequest, newItems []map[string]*dynamodb.AttributeValue) *dynamodb.BatchWriteItemInput {
	numOfRequests := len(retryItems) + len(newItems)
	writeRequests := make([]*dynamodb.WriteRequest, numOfRequests)
	for i, writeRequest := range retryItems {
		writeRequests[i] = writeRequest
	}
	for i, item := range newItems {
		writeRequest := &dynamodb.WriteRequest{PutRequest: &dynamodb.PutRequest{Item: item}}
		writeRequests[len(retryItems)+i] = writeRequest
	}
	return &dynamodb.BatchWriteItemInput{
		RequestItems: map[string][]*dynamodb.WriteRequest{
			dw.tableName: writeRequests,
		},
	}
}

func (dw *_DoneWriter) _extractUnprocessedRequests(output *dynamodb.BatchWriteItemOutput) []*dynamodb.WriteRequest {
	if output.UnprocessedItems == nil {
		return make([]*dynamodb.WriteRequest, 0)
	}
	return output.UnprocessedItems[dw.tableName]
}
