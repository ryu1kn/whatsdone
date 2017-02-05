
# copy-done-table

Copy Done table from one region to another

NOTE: New table must be ready when you run the script.

```sh
$ go build -o __main
$ ./__main \
    --from-table-name SOURCE_TALBE --from-table-region SOURCE_REGION \
    --to-table-name TARGET_TABLE --to-table-region TARGET_REGION
```
