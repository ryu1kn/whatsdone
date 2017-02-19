package main

import (
	"github.com/aws/aws-sdk-go/aws"
	"github.com/aws/aws-sdk-go/aws/session"
	"github.com/aws/aws-sdk-go/service/dynamodb"
)

type _Context struct {
	options *_CommandOptions
}

func (c *_Context) dynamoDBScanner() *_DynamoDBScanner {
	return &_DynamoDBScanner{
		dynamodb.New(session.New(&aws.Config{Region: aws.String(c.options.fromTableRegion)})),
	}
}

func (c *_Context) doneReader() *_DoneReader {
	return &_DoneReader{
		scanner:   c.dynamoDBScanner(),
		tableName: c.options.fromTableName,
	}
}

func (c *_Context) dynamoDBBatchWriter() *_DynamoDBBatchWriter {
	return &_DynamoDBBatchWriter{
		dynamodb.New(session.New(&aws.Config{Region: aws.String(c.options.toTableRegion)})),
	}
}

func (c *_Context) doneWriter() *_DoneWriter {
	return &_DoneWriter{
		dynamoDBBatchWriter: c.dynamoDBBatchWriter(),
		tableName:           c.options.toTableName,
	}
}
