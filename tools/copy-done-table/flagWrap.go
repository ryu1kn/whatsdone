package main

import "flag"

type _IFlagWrap interface {
	String(string, string, string) *string
	Parse()
}

type _FlagWrap struct{}

func (f _FlagWrap) String(argName, defaultValue, description string) *string {
	return flag.String(argName, defaultValue, description)
}

func (f *_FlagWrap) Parse() {
	flag.Parse()
}
