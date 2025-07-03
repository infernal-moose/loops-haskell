setup:
	stack build
.PHONY: setup

test:
	stack test
.PHONY: test

lint:
	stack run hlint .
.PHONY: lint

format:
	stack run ormolu
.PHONY: format
