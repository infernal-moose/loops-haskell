setup:
	@command -v pre-commit >/dev/null 2>&1 && \
	  pre-commit install || \
	  echo "⚠️  pre-commit not found - skipping hook installation"
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
