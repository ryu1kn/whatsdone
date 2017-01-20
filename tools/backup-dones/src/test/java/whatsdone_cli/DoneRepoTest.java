package whatsdone_cli;

import static org.junit.Assert.*;
import static org.mockito.Mockito.when;

import org.junit.Test;
import org.mockito.Mockito;

import com.amazonaws.services.dynamodbv2.document.ItemCollection;
import com.amazonaws.services.dynamodbv2.document.ScanOutcome;
import com.amazonaws.services.dynamodbv2.document.api.ScanApi;

public class DoneRepoTest {

    @Test
    public void test_it_loads_all_items_as_json() {
        @SuppressWarnings("unchecked")
        ItemCollection<ScanOutcome> items = Mockito.mock(ItemCollection.class);

        ScanApi scanApi = Mockito.mock(ScanApi.class);
        when(scanApi.scan()).thenReturn(items);
        ScanResultJsonFormatter formatter = Mockito.mock(ScanResultJsonFormatter.class);
        when(formatter.format(items)).thenReturn("JSON");

        DoneRepo doneRepo = new DoneRepo(scanApi, formatter);
        assertEquals(doneRepo.getAll(), "JSON");
    }

}
