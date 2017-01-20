package whatsdone_cli;

import java.util.stream.Collectors;
import java.util.stream.StreamSupport;

import com.amazonaws.services.dynamodbv2.document.Item;

public class ScanResultJsonFormatter {

    String format(Iterable<Item> items) {
        String contents = StreamSupport.stream(items.spliterator(), false)
                            .map(item -> item.toJSON())
                            .collect(Collectors.joining(","));
        return "[" + contents + "]";
    }

}
