package main

import (
	"testing"

	"github.com/aws/aws-sdk-go/service/dynamodb"
)

func TestWrite_Write1Item(t *testing.T) {
	fakeBatchWriter := _NewFakeBatchWriter()
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

	requestItems := fakeBatchWriter._inputs[0].RequestItems
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

func TestWrite_Write2Items(t *testing.T) {
	fakeBatchWriter := _NewFakeBatchWriter()
	doneWriter := &_DoneWriter{
		tableName:           "TABLE_NAME",
		dynamoDBBatchWriter: &fakeBatchWriter,
	}
	doneItems := createDoneItems(2)
	err := doneWriter.write(&doneItems)
	if err != nil {
		t.Fatal(err)
	}

	requestItems := fakeBatchWriter._inputs[0].RequestItems
	writeRequests := requestItems["TABLE_NAME"]
	if len(writeRequests) != 2 {
		t.Fatalf("Expected 2, but got \"%d\"", len(writeRequests))
	}
}

func TestWrite_WriteMoreThan25Items(t *testing.T) {
	fakeBatchWriter := _NewFakeBatchWriter()
	doneWriter := &_DoneWriter{
		tableName:           "TABLE_NAME",
		dynamoDBBatchWriter: &fakeBatchWriter,
	}
	numOfItems := 52
	doneItems := createDoneItems(numOfItems)
	err := doneWriter.write(&doneItems)
	if err != nil {
		t.Fatal(err)
	}

	requestItems1 := fakeBatchWriter._inputs[0].RequestItems
	requestItems2 := fakeBatchWriter._inputs[1].RequestItems
	requestItems3 := fakeBatchWriter._inputs[2].RequestItems
	if numOfRequests := len(requestItems1["TABLE_NAME"]); numOfRequests != 25 {
		t.Fatalf("Expected 25, but got %d", numOfRequests)
	}
	if numOfRequests := len(requestItems2["TABLE_NAME"]); numOfRequests != 25 {
		t.Fatalf("Expected 25, but got %d", numOfRequests)
	}
	if numOfRequests := len(requestItems3["TABLE_NAME"]); numOfRequests != 2 {
		t.Fatalf("Expected 2, but got %d", numOfRequests)
	}
}

func createDoneItems(numOfItems int) []map[string]*dynamodb.AttributeValue {
	items := make([]map[string]*dynamodb.AttributeValue, numOfItems)
	for i := 0; i < numOfItems; i++ {
		items[i] = map[string]*dynamodb.AttributeValue{"SOME_KEY": &dynamodb.AttributeValue{}}
	}
	return items
}

type _FakeBatchWriter struct {
	_count  int
	_inputs []*dynamodb.BatchWriteItemInput
}

func _NewFakeBatchWriter() _FakeBatchWriter {
	return _FakeBatchWriter{
		_count:  0,
		_inputs: make([]*dynamodb.BatchWriteItemInput, 3),
	}
}

func (bw *_FakeBatchWriter) BatchWriteItem(input *dynamodb.BatchWriteItemInput) (*dynamodb.BatchWriteItemOutput, error) {
	bw._inputs[bw._count] = input
	bw._count++
	return nil, nil
}
