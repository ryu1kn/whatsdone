package whatsdone_cli;

import java.util.stream.Collectors;
import java.util.stream.StreamSupport;

import com.amazonaws.AmazonClientException;
import com.amazonaws.AmazonServiceException;
import com.amazonaws.auth.AWSCredentials;
import com.amazonaws.auth.profile.ProfileCredentialsProvider;
import com.amazonaws.regions.Region;
import com.amazonaws.regions.Regions;
import com.amazonaws.services.dynamodbv2.AmazonDynamoDBClient;
import com.amazonaws.services.dynamodbv2.document.ItemCollection;
import com.amazonaws.services.dynamodbv2.document.ScanOutcome;
import com.amazonaws.services.dynamodbv2.document.Table;
import com.amazonaws.services.dynamodbv2.document.api.ScanApi;

public class DoneRepo {

    // From AWS Sample Code
    public void printTableContents() throws Exception {
        AWSCredentials credentials = null;
        try {
            credentials = new ProfileCredentialsProvider().getCredentials();
        } catch (Exception e) {
            throw new AmazonClientException(
                    "Cannot load the credentials from the credential profiles file. " +
                    "Please make sure that your credentials file is at the correct " +
                    "location (~/.aws/credentials), and is in valid format.",
                    e);
        }
        AmazonDynamoDBClient dynamoDB = new AmazonDynamoDBClient(credentials);
        Region region = Region.getRegion(Regions.AP_SOUTHEAST_2);
        dynamoDB.setRegion(region);

        try {
            String tableName = "ryuichi-test-db";
            ScanApi scanApi = new Table(dynamoDB, tableName);
            System.out.println(convertToJson(scanApi.scan()));
        } catch (AmazonServiceException ase) {
            System.out.println("Caught an AmazonServiceException, which means your request made it "
                    + "to AWS, but was rejected with an error response for some reason.");
            System.out.println("Error Message:    " + ase.getMessage());
            System.out.println("HTTP Status Code: " + ase.getStatusCode());
            System.out.println("AWS Error Code:   " + ase.getErrorCode());
            System.out.println("Error Type:       " + ase.getErrorType());
            System.out.println("Request ID:       " + ase.getRequestId());
        } catch (AmazonClientException ace) {
            System.out.println("Caught an AmazonClientException, which means the client encountered "
                    + "a serious internal problem while trying to communicate with AWS, "
                    + "such as not being able to access the network.");
            System.out.println("Error Message: " + ace.getMessage());
        }
    }

    private String convertToJson(ItemCollection<ScanOutcome> items) {
        String contents = StreamSupport.stream(items.spliterator(), false)
                            .map(item -> item.toJSON())
                            .collect(Collectors.joining(","));
        return "[" + contents + "]";
    }

    String getAll() {
        return null;
    }

}
