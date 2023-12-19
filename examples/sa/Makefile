EXAMPLES = ackermann color test todo tribute
LINT=../../bin/lint

all: $(EXAMPLES)

ackermann: build
	$(LINT) < ackermann.sa

color: build
	$(LINT) < color.sa

test: build
	$(LINT) < test.sa

todo: build
	$(LINT) < todo.sa

tribute: build
	$(LINT) < tribute.sa

build:
	(cd ../../src/lint; $(MAKE) all)

clean:
