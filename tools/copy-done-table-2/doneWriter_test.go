package main

import (
	"testing"

	"fmt"

	"github.com/aws/aws-sdk-go/service/dynamodb"
)

func TestWrite_Write1Item(t *testing.T) {
	fakeBatchWriter := _FakeBatchWriterWriteItems{}
	doneWriter := &_DoneWriter{
		tableName:           "TABLE_NAME",
		dynamoDBBatchWriter: &fakeBatchWriter,
	}
	doneItem := map[string]*dynamodb.AttributeValue{"SOME_KEY": &dynamodb.AttributeValue{}}
	doneItems := []map[string]*dynamodb.AttributeValue{doneItem}
	err := doneWriter.write(doneItems)
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

func TestWrite_Write2Items(t *testing.T) {
	fakeBatchWriter := _FakeBatchWriterWriteItems{}
	doneWriter := &_DoneWriter{
		tableName:           "TABLE_NAME",
		dynamoDBBatchWriter: &fakeBatchWriter,
	}
	doneItems := createDoneItems(2)
	err := doneWriter.write(doneItems)
	if err != nil {
		t.Fatal(err)
	}

	requestItems := fakeBatchWriter._input.RequestItems
	writeRequests := requestItems["TABLE_NAME"]
	if len(writeRequests) != 2 {
		t.Fatalf("Expected 2, but got \"%d\"", len(writeRequests))
	}
}

type _FakeBatchWriterWriteItems struct {
	_input *dynamodb.BatchWriteItemInput
}

func (bw *_FakeBatchWriterWriteItems) BatchWriteItem(input *dynamodb.BatchWriteItemInput) (*dynamodb.BatchWriteItemOutput, error) {
	bw._input = input
	return &dynamodb.BatchWriteItemOutput{}, nil
}

func TestWrite_WriteMoreThan25Items(t *testing.T) {
	fakeBatchWriter := _FakeBatchWriterWriteMoreThan25Items{
		_count:  0,
		_inputs: make([]*dynamodb.BatchWriteItemInput, 3),
	}
	doneWriter := &_DoneWriter{
		tableName:           "TABLE_NAME",
		dynamoDBBatchWriter: &fakeBatchWriter,
	}
	doneItems := createDoneItems(52)
	err := doneWriter.write(doneItems)
	if err != nil {
		t.Fatal(err)
	}

	requestItems1 := fakeBatchWriter._inputs[0].RequestItems
	if numOfRequests := len(requestItems1["TABLE_NAME"]); numOfRequests != 25 {
		t.Fatalf("Expected 25, but got %d", numOfRequests)
	}
	requestItems2 := fakeBatchWriter._inputs[1].RequestItems
	if numOfRequests := len(requestItems2["TABLE_NAME"]); numOfRequests != 25 {
		t.Fatalf("Expected 25, but got %d", numOfRequests)
	}
	requestItems3 := fakeBatchWriter._inputs[2].RequestItems
	if numOfRequests := len(requestItems3["TABLE_NAME"]); numOfRequests != 2 {
		t.Fatalf("Expected 2, but got %d", numOfRequests)
	}
}

type _FakeBatchWriterWriteMoreThan25Items struct {
	_count  int
	_inputs []*dynamodb.BatchWriteItemInput
}

func (bw *_FakeBatchWriterWriteMoreThan25Items) BatchWriteItem(input *dynamodb.BatchWriteItemInput) (*dynamodb.BatchWriteItemOutput, error) {
	bw._inputs[bw._count] = input
	bw._count++
	return &dynamodb.BatchWriteItemOutput{}, nil
}

func TestWrite_ResendUnprocessedItems(t *testing.T) {
	fakeBatchWriter := _FakeBatchWriterUnprocessedItems{
		_count:  0,
		_inputs: make([]*dynamodb.BatchWriteItemInput, 2),
	}
	doneWriter := &_DoneWriter{
		tableName:           "TABLE_NAME",
		dynamoDBBatchWriter: &fakeBatchWriter,
	}
	doneItems := createDoneItems(26)
	err := doneWriter.write(doneItems)
	if err != nil {
		t.Fatal(err)
	}

	requestItems := fakeBatchWriter._inputs[1].RequestItems
	writeRequests := requestItems["TABLE_NAME"]
	if actualItem := (*writeRequests[0]).PutRequest.Item["SOME_KEY_02"]; actualItem == nil {
		t.Fatal("Expected non nil, but got nil")
	}
}

type _FakeBatchWriterUnprocessedItems struct {
	_count  int
	_inputs []*dynamodb.BatchWriteItemInput
}

func (bw *_FakeBatchWriterUnprocessedItems) BatchWriteItem(input *dynamodb.BatchWriteItemInput) (*dynamodb.BatchWriteItemOutput, error) {
	bw._inputs[bw._count] = input
	bw._count++
	if bw._count != 1 {
		return &dynamodb.BatchWriteItemOutput{}, nil
	}

	failedWriteRequests := input.RequestItems["TABLE_NAME"][2:3]
	output := dynamodb.BatchWriteItemOutput{
		UnprocessedItems: map[string][]*dynamodb.WriteRequest{
			"TABLE_NAME": failedWriteRequests,
		},
	}
	return &output, nil
}

func TestWrite_SendUnprocessedItemsIfThereIsAny(t *testing.T) {
	fakeBatchWriter := _FakeBatchWriterUnprocessedItems2{
		_count:  0,
		_inputs: make([]*dynamodb.BatchWriteItemInput, 2),
	}
	doneWriter := &_DoneWriter{
		tableName:           "TABLE_NAME",
		dynamoDBBatchWriter: &fakeBatchWriter,
	}
	doneItems := createDoneItems(1)
	err := doneWriter.write(doneItems)
	if err != nil {
		t.Fatal(err)
	}

	requestItems := fakeBatchWriter._inputs[1].RequestItems
	writeRequests := requestItems["TABLE_NAME"]
	if actualItem := (*writeRequests[0]).PutRequest.Item["SOME_KEY_00"]; actualItem == nil {
		t.Fatal("Expected non nil, but got nil")
	}
}

type _FakeBatchWriterUnprocessedItems2 struct {
	_count  int
	_inputs []*dynamodb.BatchWriteItemInput
}

func (bw *_FakeBatchWriterUnprocessedItems2) BatchWriteItem(input *dynamodb.BatchWriteItemInput) (*dynamodb.BatchWriteItemOutput, error) {
	bw._inputs[bw._count] = input
	bw._count++
	if bw._count != 1 {
		return &dynamodb.BatchWriteItemOutput{}, nil
	}

	failedWriteRequests := input.RequestItems["TABLE_NAME"][0:1]
	output := dynamodb.BatchWriteItemOutput{
		UnprocessedItems: map[string][]*dynamodb.WriteRequest{
			"TABLE_NAME": failedWriteRequests,
		},
	}
	return &output, nil
}

func createDoneItems(numOfItems int) []map[string]*dynamodb.AttributeValue {
	items := make([]map[string]*dynamodb.AttributeValue, numOfItems)
	for i := 0; i < numOfItems; i++ {
		keyName := fmt.Sprintf("SOME_KEY_%02d", i)
		items[i] = map[string]*dynamodb.AttributeValue{keyName: &dynamodb.AttributeValue{}}
	}
	return items
}
