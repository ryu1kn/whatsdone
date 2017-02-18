package main

import (
	"fmt"
	"log"
)

func main() {
	parser := _ArgsParser{&_FlagWrap{}}
	opts := parser.parse()

	fmt.Printf("Copying \"%s\" (%s) -> \"%s\" (%s) ...\n",
		opts.fromTableName, opts.fromTableRegion, opts.toTableName, opts.toTableRegion)

	context := _Context{options: opts}

	reader := context.doneReader()
	doneCollection, err := reader.readAll()
	if err != nil {
		log.Println(err)
		return
	}

	writer := context.doneWriter()
	if err := writer.write(doneCollection); err != nil {
		log.Println(err)
	}
}
