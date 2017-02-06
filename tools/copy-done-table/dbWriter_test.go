package main

import (
	"errors"
	"fmt"
	"testing"

	"github.com/aws/aws-sdk-go/service/dynamodb"
)

func TestWrite(t *testing.T) {
	var writer _IDoneWriter = &_DoneWriter{
		batchWriter: &_FakeBatchWriter{},
		tableName:   "TABLE_NAME",
	}

	fakeValue := "FAKE_VALUE"

	// TODO: Use mock value for DoneItem instead of map[string]*dynamodb.AttributeValue
	dones := &[]_IDoneItem{map[string]*dynamodb.AttributeValue{
		"FAKE_KEY": &dynamodb.AttributeValue{S: &fakeValue},
	}}
	if err := writer.write(dones); err != nil {
		t.Error(err.Error())
	}
}

type _FakeBatchWriter struct{}

func (client *_FakeBatchWriter) BatchWriteItem(input *dynamodb.BatchWriteItemInput) error {
	writeRequests := input.RequestItems["TABLE_NAME"]
	if writeRequests == nil {
		return errors.New("Didn't get the expect table name TABLE_NAME")
	}
	item := writeRequests[0].PutRequest.Item
	if actual := *item["FAKE_KEY"].S; actual != "FAKE_VALUE" {
		return fmt.Errorf("Expected \"FAKE_VALUE\", but got \"%s\"", actual)
	}
	return nil
}
