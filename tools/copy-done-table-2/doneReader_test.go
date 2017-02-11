package main

import "testing"

func TestFetchAllItems(t *testing.T) {
	items := []_DoneItem{_DoneItem{"FAKE_VALUE"}}
	fakeResult := _ReadAllResult{&items}
	scanner := _Scanner{result: fakeResult}
	reader := &_DoneReader{
		tableName: "TABLE_NAME",
		scanner:   &scanner,
	}
	result, _ := reader.readAll()
	if tableName := scanner.spyTableName; tableName != "TABLE_NAME" {
		t.Errorf("Expected \"TABLE_NAME\", but got \"%s\"", tableName)
	}
	if count := result.count(); count != 1 {
		t.Errorf("Expected 1 item, got \"%s\" item(s)", count)
	}
	if value := (*result.items())[0].value; value != "FAKE_VALUE" {
		t.Errorf("Expected \"FAKE_VALUE\", got \"%s\"", value)
	}
}

func TestFetchAllItems_MultipleItems(t *testing.T) {
	items := []_DoneItem{_DoneItem{"FAKE_VALUE_1"}, _DoneItem{"FAKE_VALUE_2"}}
	fakeResult := _ReadAllResult{&items}
	scanner := _Scanner{result: fakeResult}
	reader := &_DoneReader{scanner: &scanner}
	result, _ := reader.readAll()
	if count := result.count(); count != 2 {
		t.Errorf("Expected 2 item, got \"%s\" item(s)", count)
	}
	if value := (*result.items())[0].value; value != "FAKE_VALUE_1" {
		t.Errorf("Expected \"FAKE_VALUE\", got \"%s\"", value)
	}
}

func TestFetchAllItems_DifferentTable(t *testing.T) {
	fakeResult := _ReadAllResult{&[]_DoneItem{}}
	scanner := _Scanner{result: fakeResult}
	reader := &_DoneReader{
		tableName: "TABLE_NAME_2",
		scanner:   &scanner,
	}
	reader.readAll()
	if tableName := scanner.spyTableName; tableName != "TABLE_NAME_2" {
		t.Errorf("Expected \"TABLE_NAME_2\", but got \"%s\"", tableName)
	}
}

type _Scanner struct {
	result       _ReadAllResult
	spyTableName string
}

func (s *_Scanner) Scan(input string) (*_ReadAllResult, error) {
	s.spyTableName = input
	return &s.result, nil
}
