setup:
	stack build
.PHONY: setup

test:
	stack test
.PHONY: test

lint:
	stack exec -- hlint .
.PHONY: lint

format:
	stack exec -- fourmolu --mode inplace src/ test/
.PHONY: format
