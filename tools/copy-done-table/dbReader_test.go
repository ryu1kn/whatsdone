package main

import (
	"testing"

	"github.com/aws/aws-sdk-go/service/dynamodb"
)

func TestRead(t *testing.T) {
	var reader DoneReader = &DoneReaderImpl{
		scanner:   &fakeScanner{},
		tableName: "TABLE_NAME",
	}

	expected := &[]DoneItem{fakeDoneItem{"FAKE_ITEM"}}
	actual, _ := reader.read()

	if len(*(*actual).Items()) != len(*expected) {
		t.Error("Expected/actual number of elements doesn't match")
	}
	for i, aValue := range *(*actual).Items() {
		if eValue := (*expected)[i]; aValue != eValue {
			t.Errorf("Expected \"%s\", but got \"%s\"", eValue, aValue)
		}
	}
}

type fakeScanner struct{}

type fakeDynamodbScanOutput struct{}

func (output fakeDynamodbScanOutput) Items() *[]DoneItem {
	return &[]DoneItem{fakeDoneItem{"FAKE_ITEM"}}
}

type fakeDoneItem struct {
	raw string
}

func (client *fakeScanner) Scan(input *dynamodb.ScanInput) (*dynamodbScanOutput, error) {
	if *(input.TableName) != "TABLE_NAME" {
		return nil, nil
	}

	var output dynamodbScanOutput = &fakeDynamodbScanOutput{}
	return &output, nil
}
