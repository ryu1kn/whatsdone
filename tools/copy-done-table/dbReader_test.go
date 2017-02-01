package main

import (
	"testing"

	"github.com/aws/aws-sdk-go/aws"
	"github.com/aws/aws-sdk-go/service/dynamodb"
)

// WIP: Test not working
func TestRead(t *testing.T) {
	var fakeReader DoneReader = &DoneReaderImpl{
		scanner:   &fakeScanner{},
		tableName: "TABLE_NAME",
	}

	actual, _ := fakeReader.read()
	expected := &[]DoneItem{
		&DoneItemImpl{},
	}

	if len(*actual) != len(*expected) {
		t.Error("Expected/actual number of elements doesn't match")
	}
	for i, aValue := range *actual {
		if eValue := (*expected)[i]; aValue != eValue {
			t.Errorf("Expected \"%s\", but got \"%s\"", eValue, aValue)
		}
	}
}

type fakeScanner struct{}

func (client *fakeScanner) Scan(input *dynamodb.ScanInput) (*dynamodb.ScanOutput, error) {
	if *(input.TableName) == "TABLE_NAME" {
		return &dynamodb.ScanOutput{
			Items: []map[string]*dynamodb.AttributeValue{
				map[string]*dynamodb.AttributeValue{
					"FAKE_KEY": &dynamodb.AttributeValue{
						S: aws.String("FAKE_VALUE"),
					},
				},
			},
		}, nil
	}
	return nil, nil
}
