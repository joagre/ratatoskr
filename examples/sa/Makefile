EXAMPLES = ackermann btree-class btree color test todo tribute
LINT=../../bin/lint

all: $(EXAMPLES)

ackermann: build
	$(LINT) < ackermann.sa

btree-class: build
	$(LINT) < btree-class.sa

btree: build
	$(LINT) < btree.sa

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
