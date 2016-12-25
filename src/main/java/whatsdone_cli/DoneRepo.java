package whatsdone_cli;

import java.util.stream.Collectors;
import java.util.stream.StreamSupport;

import com.amazonaws.services.dynamodbv2.AmazonDynamoDBClient;
import com.amazonaws.services.dynamodbv2.document.ItemCollection;
import com.amazonaws.services.dynamodbv2.document.ScanOutcome;
import com.amazonaws.services.dynamodbv2.document.Table;
import com.amazonaws.services.dynamodbv2.document.api.ScanApi;

public class DoneRepo {

    private AmazonDynamoDBClient dynamoDB;

    DoneRepo(AmazonDynamoDBClient dynamoDB) {
        this.dynamoDB = dynamoDB;
    }

    String getAll() {
        String tableName = "ryuichi-test-db";
        ScanApi scanApi = new Table(dynamoDB, tableName);
        return convertToJson(scanApi.scan());
    }

    private String convertToJson(ItemCollection<ScanOutcome> items) {
        String contents = StreamSupport.stream(items.spliterator(), false)
                            .map(item -> item.toJSON())
                            .collect(Collectors.joining(","));
        return "[" + contents + "]";
    }

}
