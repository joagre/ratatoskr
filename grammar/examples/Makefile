all: ackermann color test todo tribute

ackermann: build
	../lint < ackermann.sa

color: build
	../lint < color.sa

test: build
	../lint < test.sa

todo: build
	../lint < todo.sa

tribute: build
	../lint < tribute.sa

build:
	(cd ../; $(MAKE))

.PHONY: all build ackermann color test todo tribute
