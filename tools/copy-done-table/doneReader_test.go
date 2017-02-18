package main

import (
	"testing"

	"github.com/aws/aws-sdk-go/service/dynamodb"
)

func TestFetchAllItems(t *testing.T) {
	scanner := _FakeScanner{fakeResult: "FAKE_VALUE"}
	reader := &_DoneReader{
		tableName: "TABLE_NAME",
		scanner:   &scanner,
	}
	var (
		result *_DoneCollection
		err    error
	)
	result, err = reader.readAll()
	if err != nil {
		t.Fatal(err)
	}
	if tableName := scanner.spyTableName; tableName != "TABLE_NAME" {
		t.Fatalf("Expected \"TABLE_NAME\", but got \"%s\"", tableName)
	}
	if items := (*result).items; len(items) != 0 { // Shouldn't I assert `result` somehow?
		t.Fatalf("Expected 0, got \"%s\"", len(items))
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
		t.Fatalf("Expected \"TABLE_NAME_2\", but got \"%s\"", tableName)
	}
}

type _FakeScanner struct {
	fakeResult   string
	spyTableName string
}

func (s *_FakeScanner) scan(input *dynamodb.ScanInput) (*dynamodb.ScanOutput, error) {
	s.spyTableName = *input.TableName

	fakeOutput := dynamodb.ScanOutput{
		Items: make([]map[string]*dynamodb.AttributeValue, 0),
	}
	return &fakeOutput, nil
}
