package main

type _DoneReader struct {
	tableName string
	scanner   *_Scanner
}

func (r *_DoneReader) readAll() (*_ReadAllResult, error) {
	return r.scanner.Scan(r.tableName)
}

type _ReadAllResult struct {
	_items *[]_DoneItem
}

func (r *_ReadAllResult) count() int {
	return len(*r.items())
}

func (r *_ReadAllResult) items() *[]_DoneItem {
	return r._items
}

type _DoneItem struct {
	value string
}
