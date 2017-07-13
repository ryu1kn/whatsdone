
CONFIG := __config.json
ARTIFACT := .stack-work/dist/*/*/build/api-client-exe/api-client-exe
ARTIFACT_COMMAND := apiClient

build:
	stack build
	cp $(ARTIFACT) $(ARTIFACT_COMMAND)

clean:
	rm -f $(ARTIFACT_COMMAND)
	rm -f `jq .sessionFile $(CONFIG) --raw-output`
	rm -f __output.txt
