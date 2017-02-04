package main

import (
	"flag"
	"fmt"
	"log"

	"github.com/aws/aws-sdk-go/aws"
	"github.com/aws/aws-sdk-go/aws/session"
	"github.com/aws/aws-sdk-go/service/dynamodb"
)

const batchWriteMaxItemCount = 25

func main() {
	opts := parseCommandOptions()

	fmt.Printf("Copying \"%s\" (%s) -> \"%s\" (%s) ...\n",
		opts.fromTableName, opts.fromTableRegion, opts.toTableName, opts.toTableRegion)

	dynamoClientFrom := &_DynamoDBClient{
		*dynamodb.New(session.New(&aws.Config{Region: aws.String(opts.fromTableRegion)})),
	}
	reader := &_DoneReader{
		scanner:   dynamoClientFrom,
		tableName: opts.fromTableName,
	}

	scanOutput, err := reader.read()
	if err != nil {
		log.Println(err)
		return
	}

	var dynamoClientTo _IDynamoDBWriter = &_DynamoDBClient{
		*dynamodb.New(session.New(&aws.Config{Region: aws.String(opts.toTableRegion)})),
	}

	err = writeItems(dynamoClientTo, opts.toTableName, (*scanOutput).Items())
	if err != nil {
		log.Println(err)
	}
}

type _CommandOptions struct {
	fromTableName, fromTableRegion, toTableName, toTableRegion string
}

func parseCommandOptions() *_CommandOptions {
	fromTableName := flag.String("from-table-name", "", "Table name of the copy source")
	fromTableRegion := flag.String("from-table-region", "ap-southeast-2", "Table region of the copy source")
	toTableName := flag.String("to-table-name", "", "Table name of the copy target")
	toTableRegion := flag.String("to-table-region", "ap-southeast-2", "Table region of the copy target")
	flag.Parse()

	return &_CommandOptions{*fromTableName, *fromTableRegion, *toTableName, *toTableRegion}
}

type _DynamoDBClient struct {
	client dynamodb.DynamoDB
}

func (d *_DynamoDBClient) Scan(input *dynamodb.ScanInput) (*_IDynamoDBScanOutput, error) {
	scanOutput, error := d.client.Scan(input)
	var output _IDynamoDBScanOutput = &_DynamoDBScanner{scanOutput}
	return &output, error
}

func (d *_DynamoDBClient) BatchWriteItem(input *dynamodb.BatchWriteItemInput) (*dynamodb.BatchWriteItemOutput, error) {
	return d.client.BatchWriteItem(input)
}

func writeItems(dc _IDynamoDBWriter, toTableName string, doneItems *[]_IDoneItem) error {
	items := make([]map[string]*dynamodb.AttributeValue, len(*doneItems))
	for i, doneItem := range *doneItems {
		items[i] = doneItem.(map[string]*dynamodb.AttributeValue)
	}

	itemCount := len(items)

	for i := 0; i < itemCount; i += batchWriteMaxItemCount {
		var subList []map[string]*dynamodb.AttributeValue

		if i+batchWriteMaxItemCount >= itemCount {
			subList = items[i:]
		} else {
			subList = items[i : i+batchWriteMaxItemCount]
		}

		writeRequest := make([]*dynamodb.WriteRequest, len(subList))
		for idx, item := range subList {
			writeRequest[idx] = &dynamodb.WriteRequest{
				PutRequest: &dynamodb.PutRequest{Item: item},
			}
		}

		// XXX: result.UnprocessedItems is not being taken care of
		_, error := dc.BatchWriteItem(&dynamodb.BatchWriteItemInput{
			RequestItems: map[string][]*dynamodb.WriteRequest{toTableName: writeRequest},
		})
		if error != nil {
			return error
		}
	}

	return nil
}
