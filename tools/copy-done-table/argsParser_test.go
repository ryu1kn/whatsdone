package main

import "testing"

func TestParse(t *testing.T) {
	flagWrap := _FakeFlagWrap{}
	parser := &_ArgsParser{&flagWrap}
	options := parser.parse()

	if value := options.fromTableName; value != "FROM_TABLE_NAME" {
		t.Errorf("Expected \"FROM_TABLE_NAME\", but got \"%s\"", value)
	}
	if value := options.fromTableRegion; value != "FROM_TABLE_REGION" {
		t.Errorf("Expected \"FROM_TABLE_REGION\", but got \"%s\"", value)
	}
	if value := options.toTableName; value != "TO_TABLE_NAME" {
		t.Errorf("Expected \"TO_TABLE_NAME\", but got \"%s\"", value)
	}
	if value := options.toTableRegion; value != "TO_TABLE_REGION" {
		t.Errorf("Expected \"TO_TABLE_REGION\", but got \"%s\"", value)
	}
	if !flagWrap.parseCalled {
		t.Errorf("Expected \"flagWrap.Parse\" to be called")
	}
}

type _FakeFlagWrap struct {
	parseCalled bool
}

func (f _FakeFlagWrap) String(argName, defaultValue, description string) *string {
	var argValue string
	switch argName {
	case "from-table-name":
		argValue = "FROM_TABLE_NAME"
	case "from-table-region":
		argValue = "FROM_TABLE_REGION"
	case "to-table-name":
		argValue = "TO_TABLE_NAME"
	case "to-table-region":
		argValue = "TO_TABLE_REGION"
	}
	return &argValue
}

func (f *_FakeFlagWrap) Parse() {
	f.parseCalled = true
}
