package main

type _DoneReader struct {
}

func (r *_DoneReader) readAll() *_ReadAllResult {
	return &_ReadAllResult{}
}

type _ReadAllResult struct {
}

func (r *_ReadAllResult) count() int {
	return len(*r.items())
}

func (r *_ReadAllResult) items() *[]_DoneItem {
	items := make([]_DoneItem, 1)
	items[0] = _DoneItem{"FAKE_VALUE"}
	return &items
}

type _DoneItem struct {
	value string
}
