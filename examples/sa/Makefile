SOURCES := $(wildcard *.sa)
LINT := ../../bin/lint
LINT_SOURCE_DIR := ../../src/lint

lint: build
	@$(foreach src,$(SOURCES),echo "Linting $(src)"; $(LINT) < $(src) || exit 1;)

build:
	(cd $(LINT_SOURCE_DIR); $(MAKE) all)

clean:

.PHONY: lint build clean
