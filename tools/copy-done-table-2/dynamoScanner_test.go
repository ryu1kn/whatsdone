package main

import (
	"testing"

	"fmt"

	"github.com/aws/aws-sdk-go/aws"
	"github.com/aws/aws-sdk-go/aws/session"
	"github.com/aws/aws-sdk-go/service/dynamodb"
)

// Integration test
func TestScan_Integration(t *testing.T) {
	dynamoClient := dynamodb.New(session.New(&aws.Config{Region: aws.String("ap-southeast-2")}))
	scanner := _Scanner{dynamoClient}
	scanOutput, err := scanner.scan(&dynamodb.ScanInput{TableName: aws.String("ryuichi-test")})
	if err != nil {
		t.Error(err)
		return
	}

	expected := "2016-03-27T12:42:32.625Z"
	if item := (*scanOutput).Items[0]; *(item["date"].S) != expected {
		t.Errorf("Expected \"%s\", but got \"%s\"", expected, *(item["date"].S))
	}
}

func TestScan(t *testing.T) {
	dynamoClient := &_FakeDynamoDB{}
	scanner := _Scanner{dynamoClient}
	scanOutput, err := scanner.scan(&dynamodb.ScanInput{TableName: aws.String("TABLE_NAME")})
	if err != nil {
		t.Error(err)
		return
	}

	expected := "DATE_VALUE"
	if item := (*scanOutput).Items[0]; *(item["date"].S) != "DATE_VALUE" {
		t.Errorf("Expected \"%s\", but got \"%s\"", expected, *(item["date"].S))
	}
}

type _FakeDynamoDB struct{}

func (s *_FakeDynamoDB) Scan(input *dynamodb.ScanInput) (*dynamodb.ScanOutput, error) {
	expectedTableName := "TABLE_NAME"
	if *input.TableName != expectedTableName {
		return nil, fmt.Errorf("Expected \"%s\", but got \"%s\"", expectedTableName, *input.TableName)
	}
	fakeScanOutput := dynamodb.ScanOutput{
		Items: []map[string]*dynamodb.AttributeValue{
			map[string]*dynamodb.AttributeValue{
				"date": &dynamodb.AttributeValue{
					S: aws.String("DATE_VALUE"),
				},
			},
		},
	}
	return &fakeScanOutput, nil
}
