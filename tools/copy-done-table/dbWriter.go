package main

import "github.com/aws/aws-sdk-go/service/dynamodb"

type _IDoneWriter interface {
	write(*[]_IDoneItem) error
}

type _IDynamoDBBatchWriter interface {
	BatchWriteItem(*dynamodb.BatchWriteItemInput) error
}

type _DoneWriter struct {
	batchWriter _IDynamoDBBatchWriter
	tableName   string
}

func (writer *_DoneWriter) write(doneItems *[]_IDoneItem) error {
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
		error := writer.batchWriter.BatchWriteItem(&dynamodb.BatchWriteItemInput{
			RequestItems: map[string][]*dynamodb.WriteRequest{writer.tableName: writeRequest},
		})
		if error != nil {
			return error
		}
	}

	return nil
}
