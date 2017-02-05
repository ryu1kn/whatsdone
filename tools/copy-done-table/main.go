package main

import (
	"fmt"
	"log"

	"github.com/aws/aws-sdk-go/aws"
	"github.com/aws/aws-sdk-go/aws/session"
	"github.com/aws/aws-sdk-go/service/dynamodb"
)

func main() {
	parser := _ArgsParser{&_FlagWrap{}}
	opts := parser.parse()

	fmt.Printf("Copying \"%s\" (%s) -> \"%s\" (%s) ...\n",
		opts.fromTableName, opts.fromTableRegion, opts.toTableName, opts.toTableRegion)

	dynamodbScanner := &_DynamoDBScanner{
		*dynamodb.New(session.New(&aws.Config{Region: aws.String(opts.fromTableRegion)})),
	}
	reader := &_DoneReader{
		scanner:   dynamodbScanner,
		tableName: opts.fromTableName,
	}

	scanOutput, err := reader.read()
	if err != nil {
		log.Println(err)
		return
	}

	dynamodbBatchWriter := &_DynamoDBBatchWriter{
		*dynamodb.New(session.New(&aws.Config{Region: aws.String(opts.toTableRegion)})),
	}
	writer := &_DoneWriter{
		batchWriter: dynamodbBatchWriter,
		tableName:   opts.toTableName,
	}

	if err := writer.write((*scanOutput).Items()); err != nil {
		log.Println(err)
	}
}
