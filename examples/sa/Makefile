all: ackermann color test todo tribute

LINT=../../bin/lint

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
