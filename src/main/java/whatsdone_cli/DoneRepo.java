package whatsdone_cli;

import com.amazonaws.services.dynamodbv2.document.api.ScanApi;

public class DoneRepo {

    private final ScanApi scanApi;
    private final ScanResultJsonFormatter formatter;

    DoneRepo(ScanApi scanApi, ScanResultJsonFormatter formatter) {
        this.scanApi = scanApi;
        this.formatter = formatter;
    }

    String getAll() {
        return formatter.format(scanApi.scan());
    }

}
