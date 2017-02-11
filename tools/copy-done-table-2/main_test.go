package main

import "testing"

func TestFetchAllItems(t *testing.T) {
	reader := &_DoneReader{}
	result := reader.readAll()
	if count := result.count(); count != 1 {
		t.Errorf("Expected 1 item, got \"%s\" item(s)", count)
	}
	if value := (*result.items())[0].value; value != "FAKE_VALUE" {
		t.Errorf("Expected \"FAKE_VALUE\", got \"%s\"", value)
	}
}
