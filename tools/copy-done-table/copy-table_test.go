package main

import (
	"testing"

	"github.com/aws/aws-sdk-go/aws"
	"github.com/aws/aws-sdk-go/service/dynamodb"
)

func TestRetrieveAllItems(t *testing.T) {
	var fakeClient dynamodbScanner = &fakeDynamoDBClient{}
	result, _ := retrieveAllItems(fakeClient, "TABLE_NAME")
	if len(result) != 1 {
		t.Error("There should be only 1 item")
	}
	expected := "FAKE_VALUE"
	actual := *result[0]["FAKE_KEY"].S
	if actual != expected {
		t.Errorf("Expected \"%s\", but got \"%s\"", expected, actual)
	}
}

type fakeDynamoDBClient struct{}

func (client *fakeDynamoDBClient) Scan(input *dynamodb.ScanInput) (*dynamodb.ScanOutput, error) {
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
