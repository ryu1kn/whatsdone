package main

type _ArgsParser struct {
	flagWrap _IFlagWrap
}

type _CommandOptions struct {
	fromTableName, fromTableRegion, toTableName, toTableRegion string
}

func (parser _ArgsParser) parse() *_CommandOptions {
	fromTableName := parser.flagWrap.String("from-table-name", "", "Table name of the copy source")
	fromTableRegion := parser.flagWrap.String("from-table-region", "ap-southeast-2", "Table region of the copy source")
	toTableName := parser.flagWrap.String("to-table-name", "", "Table name of the copy target")
	toTableRegion := parser.flagWrap.String("to-table-region", "ap-southeast-2", "Table region of the copy target")
	parser.flagWrap.Parse()

	return &_CommandOptions{*fromTableName, *fromTableRegion, *toTableName, *toTableRegion}
}
