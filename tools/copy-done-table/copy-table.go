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
		opts["fromTableName"], opts["fromTableRegion"], opts["toTableName"], opts["toTableRegion"])

	dynamoClientFrom := dynamodb.New(session.New(&aws.Config{Region: aws.String(opts["fromTableRegion"])}))
	items, err := retrieveAllItems(dynamoClientFrom, opts["fromTableName"])
	if err != nil {
		log.Println(err)
		return
	}

	dynamoClientTo := dynamodb.New(session.New(&aws.Config{Region: aws.String(opts["toTableRegion"])}))
	err = writeItems(dynamoClientTo, opts["toTableName"], &items)
	if err != nil {
		log.Println(err)
	}
}

func parseCommandOptions() map[string]string {
	fromTableName := flag.String("from-table-name", "", "Table name of the copy source")
	fromTableRegion := flag.String("from-table-region", "ap-southeast-2", "Table region of the copy source")
	toTableName := flag.String("to-table-name", "", "Table name of the copy target")
	toTableRegion := flag.String("to-table-region", "ap-southeast-2", "Table region of the copy target")
	flag.Parse()

	return map[string]string{
		"fromTableName":   *fromTableName,
		"fromTableRegion": *fromTableRegion,
		"toTableName":     *toTableName,
		"toTableRegion":   *toTableRegion,
	}
}

func retrieveAllItems(dynamoClient *dynamodb.DynamoDB, tableName string) ([]map[string]*dynamodb.AttributeValue, error) {
	result, err := dynamoClient.Scan(&dynamodb.ScanInput{TableName: &tableName})
	if err != nil {
		return nil, err
	}
	return result.Items, nil
}

func writeItems(dynamoClient *dynamodb.DynamoDB, toTableName string, items *[]map[string]*dynamodb.AttributeValue) error {
	itemCount := len(*items)

	for i := 0; i < itemCount; i += batchWriteMaxItemCount {
		var subList []map[string]*dynamodb.AttributeValue

		if i+batchWriteMaxItemCount >= itemCount {
			subList = (*items)[i:]
		} else {
			subList = (*items)[i : i+batchWriteMaxItemCount]
		}

		writeRequest := make([]*dynamodb.WriteRequest, len(subList))
		for idx, item := range subList {
			writeRequest[idx] = &dynamodb.WriteRequest{
				PutRequest: &dynamodb.PutRequest{Item: item},
			}
		}

		// XXX: result.UnprocessedItems is not being taken care of
		_, error := dynamoClient.BatchWriteItem(&dynamodb.BatchWriteItemInput{
			RequestItems: map[string][]*dynamodb.WriteRequest{toTableName: writeRequest},
		})
		if error != nil {
			return error
		}
	}

	return nil
}
