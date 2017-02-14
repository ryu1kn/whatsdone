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
	doneItem := map[string]*dynamodb.AttributeValue{"SOME_KEY": &dynamodb.AttributeValue{}}
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
	if actualItem := item.PutRequest.Item["SOME_KEY"]; actualItem != doneItem["SOME_KEY"] {
		t.Fatalf("Expected \"%s\", but got \"%s\"", doneItem["SOME_KEY"], actualItem)
	}
}

func TestWrite_Write2Item(t *testing.T) {
	fakeBatchWriter := _FakeBatchWriter{}
	doneWriter := &_DoneWriter{
		tableName:           "TABLE_NAME",
		dynamoDBBatchWriter: &fakeBatchWriter,
	}
	doneItem1 := map[string]*dynamodb.AttributeValue{"SOME_KEY": &dynamodb.AttributeValue{}}
	doneItem2 := map[string]*dynamodb.AttributeValue{"SOME_KEY": &dynamodb.AttributeValue{}}
	doneItems := []map[string]*dynamodb.AttributeValue{doneItem1, doneItem2}
	err := doneWriter.write(&doneItems)
	if err != nil {
		t.Fatal(err)
	}

	requestItems := fakeBatchWriter._input.RequestItems
	writeRequests := requestItems["TABLE_NAME"]
	if len(writeRequests) != 2 {
		t.Fatalf("Expected 2, but got \"%d\"", len(writeRequests))
	}
}

type _FakeBatchWriter struct {
	_input *dynamodb.BatchWriteItemInput
}

func (bw *_FakeBatchWriter) BatchWriteItem(input *dynamodb.BatchWriteItemInput) (*dynamodb.BatchWriteItemOutput, error) {
	bw._input = input
	return nil, nil
}
