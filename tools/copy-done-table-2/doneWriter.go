package main

import "github.com/aws/aws-sdk-go/service/dynamodb"

const _MaxBatchWriteCount = 25

type _DoneWriter struct {
	tableName           string
	dynamoDBBatchWriter _IBatchWriter
}

func (dw *_DoneWriter) write(items []map[string]*dynamodb.AttributeValue) error {
	ic := newItemCollection(items)
	for writeRequests := ic.nextWriteRequests(); len(writeRequests) != 0; writeRequests = ic.nextWriteRequests() {
		input := dynamodb.BatchWriteItemInput{
			RequestItems: map[string][]*dynamodb.WriteRequest{
				dw.tableName: writeRequests,
			},
		}
		output, err := dw.dynamoDBBatchWriter.BatchWriteItem(&input)
		if err != nil {
			return err
		}
		ic.retryItems = dw._extractUnprocessedRequests(output)
	}
	return nil
}

func (dw *_DoneWriter) _extractUnprocessedRequests(output *dynamodb.BatchWriteItemOutput) []*dynamodb.WriteRequest {
	if output.UnprocessedItems == nil {
		return nil
	}
	return output.UnprocessedItems[dw.tableName]
}

func newItemCollection(items []map[string]*dynamodb.AttributeValue) itemCollection {
	return itemCollection{
		items:      items,
		retryItems: []*dynamodb.WriteRequest{},
	}
}

type itemCollection struct {
	items      []map[string]*dynamodb.AttributeValue
	retryItems []*dynamodb.WriteRequest
}

func (ic *itemCollection) nextWriteRequests() []*dynamodb.WriteRequest {
	defer func() { ic.retryItems = nil }()

	var retryWriteRequest, newWriteRequests []*dynamodb.WriteRequest

	if ic.retryItems == nil {
		retryWriteRequest = []*dynamodb.WriteRequest{}
	} else {
		retryWriteRequest = ic.retryItems
	}

	numOfItems, numOfRetryItems := len(ic.items), len(ic.retryItems)
	numOfNextNewItems := _MaxBatchWriteCount - numOfRetryItems
	if numOfItems > numOfNextNewItems {
		newItems := ic.items[:numOfNextNewItems]
		ic.items = ic.items[numOfNextNewItems:]
		newWriteRequests = ic.buildWriteRequests(newItems)
	} else {
		newItems := ic.items
		ic.items = ic.items[numOfItems:]
		newWriteRequests = ic.buildWriteRequests(newItems)
	}
	return append(retryWriteRequest, newWriteRequests...)
}

func (ic *itemCollection) buildWriteRequests(newItems []map[string]*dynamodb.AttributeValue) []*dynamodb.WriteRequest {
	writeRequests := make([]*dynamodb.WriteRequest, len(newItems))
	for i, item := range newItems {
		writeRequest := &dynamodb.WriteRequest{PutRequest: &dynamodb.PutRequest{Item: item}}
		writeRequests[i] = writeRequest
	}
	return writeRequests
}
