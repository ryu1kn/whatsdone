package main

import (
	"testing"

	"github.com/aws/aws-sdk-go/service/dynamodb"
)

func TestRead(t *testing.T) {
	var reader _IDoneReader = &_DoneReader{
		scanner:   &_FakeScanner{},
		tableName: "TABLE_NAME",
	}

	expected := &[]_IDoneItem{_FakeDoneItem{"FAKE_ITEM"}}
	actual, _ := reader.read()

	if len(*actual) != len(*expected) {
		t.Error("Expected/actual number of elements doesn't match")
	}
	for i, aValue := range *actual {
		if eValue := (*expected)[i]; aValue != eValue {
			t.Errorf("Expected \"%s\", but got \"%s\"", eValue, aValue)
		}
	}
}

type _FakeScanner struct{}

type _FakeDynamodbScanOutput struct{}

func (output _FakeDynamodbScanOutput) Items() *[]_IDynamoDBItem {
	return &[]_IDynamoDBItem{_FakeDoneItem{"FAKE_ITEM"}}
}

type _FakeDoneItem struct {
	raw string
}

func (client *_FakeScanner) Scan(input *dynamodb.ScanInput) (*_IDynamoDBScanOutput, error) {
	if *(input.TableName) != "TABLE_NAME" {
		return nil, nil
	}

	var output _IDynamoDBScanOutput = &_FakeDynamodbScanOutput{}
	return &output, nil
}
