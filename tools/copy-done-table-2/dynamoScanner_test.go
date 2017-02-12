package main

import (
	"testing"

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
