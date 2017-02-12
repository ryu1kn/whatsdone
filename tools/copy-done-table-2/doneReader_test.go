package main

import (
	"testing"

	"github.com/aws/aws-sdk-go/aws"
	"github.com/aws/aws-sdk-go/service/dynamodb"
)

func TestFetchAllItems(t *testing.T) {
	scanner := _FakeScanner{fakeResult: "FAKE_VALUE"}
	reader := &_DoneReader{
		tableName: "TABLE_NAME",
		scanner:   &scanner,
	}
	result, err := reader.readAll()
	if err != nil {
		t.Error(err)
		return
	}
	if tableName := scanner.spyTableName; tableName != "TABLE_NAME" {
		t.Errorf("Expected \"TABLE_NAME\", but got \"%s\"", tableName)
	}
	if count := *result.Count; count != 1 {
		t.Errorf("Expected 1 item, got \"%s\" item(s)", count)
	}
	if value := *result.Items[0]["FAKE_KEY"].S; value != "FAKE_VALUE" {
		t.Errorf("Expected \"FAKE_VALUE\", got \"%s\"", value)
	}
}

func TestFetchAllItems_DifferentTable(t *testing.T) {
	scanner := _FakeScanner{fakeResult: "FAKE_VALUE"}
	reader := &_DoneReader{
		tableName: "TABLE_NAME_2",
		scanner:   &scanner,
	}
	reader.readAll()
	if tableName := scanner.spyTableName; tableName != "TABLE_NAME_2" {
		t.Errorf("Expected \"TABLE_NAME_2\", but got \"%s\"", tableName)
	}
}

type _FakeScanner struct {
	fakeResult   string
	spyTableName string
}

func (s *_FakeScanner) scan(input *dynamodb.ScanInput) (*dynamodb.ScanOutput, error) {
	s.spyTableName = *input.TableName

	items := []map[string]*dynamodb.AttributeValue{
		map[string]*dynamodb.AttributeValue{
			"FAKE_KEY": &dynamodb.AttributeValue{
				S: aws.String(s.fakeResult),
			},
		},
	}
	fakeOutput := dynamodb.ScanOutput{
		Count: aws.Int64(int64(len(items))),
		Items: items,
	}
	return &fakeOutput, nil
}
