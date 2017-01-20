
package whatsdone_cli;

import com.amazonaws.auth.AWSCredentials;
import com.amazonaws.auth.profile.ProfileCredentialsProvider;
import com.amazonaws.regions.Region;
import com.amazonaws.regions.Regions;
import com.amazonaws.services.dynamodbv2.AmazonDynamoDBClient;
import com.amazonaws.services.dynamodbv2.document.Table;
import com.amazonaws.services.dynamodbv2.document.api.ScanApi;

public class Main {

    public static void main(String[] args) {
        System.out.println("App started...");
        ScanApi scanApi = createScanApi();
        ScanResultJsonFormatter formatter = new ScanResultJsonFormatter();
        DoneRepo repo = new DoneRepo(scanApi, formatter);
        System.out.println(repo.getAll());
    }

    private static AmazonDynamoDBClient createDynamoDB() {
        AWSCredentials credentials = new ProfileCredentialsProvider().getCredentials();
        AmazonDynamoDBClient dynamoDB = new AmazonDynamoDBClient(credentials);
        Region region = Region.getRegion(Regions.AP_SOUTHEAST_2);
        dynamoDB.setRegion(region);
        return dynamoDB;
    }

    private static ScanApi createScanApi() {
        String tableName = "ryuichi-test-db";
        return new Table(createDynamoDB(), tableName);
    }

}
