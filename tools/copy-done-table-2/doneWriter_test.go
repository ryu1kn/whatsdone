package main

import (
	"testing"

	"github.com/aws/aws-sdk-go/service/dynamodb"
)

func TestWrite_Write1Item(t *testing.T) {
	fakeBatchWriter := _FakeBatchWriter{}
	doneWriter := &_DoneWriter{
		tableName:           "TABLE_NAME",
		dynamoDBBatchWriter: &fakeBatchWriter,
	}
	someValue := dynamodb.AttributeValue{}
	doneItem := map[string]*dynamodb.AttributeValue{
		"SOME_KEY": &someValue,
	}
	doneItems := []map[string]*dynamodb.AttributeValue{doneItem}
	err := doneWriter.write(&doneItems)
	if err != nil {
		t.Fatal(err)
	}

	requestItems := fakeBatchWriter._input.RequestItems
	writeRequests := requestItems["TABLE_NAME"]
	if writeRequests == nil {
		t.Fatal("Expected a request to the table \"TABLE_NAME\"")
	}
	if len(writeRequests) != 1 {
		t.Fatalf("Expected 1, but got \"%d\"", len(writeRequests))
	}
	item := *writeRequests[0]
	if actualItem := item.PutRequest.Item["SOME_KEY"]; actualItem != &someValue {
		t.Fatalf("Expected \"%s\", but got \"%s\"", someValue, actualItem)
	}
}

type _FakeBatchWriter struct {
	_input *dynamodb.BatchWriteItemInput
}

func (bw *_FakeBatchWriter) BatchWriteItem(input *dynamodb.BatchWriteItemInput) (*dynamodb.BatchWriteItemOutput, error) {
	bw._input = input
	return nil, nil
}
