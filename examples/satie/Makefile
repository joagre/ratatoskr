all: ackermann color test todo tribute

ackermann: build
	../../grammar/satie < ackermann.sa

color: build
	../../grammar/satie < color.sa

test: build
	../../grammar/satie < test.sa

todo: build
	../../grammar/satie < todo.sa


tribute: build
	../../grammar/satie < tribute.sa

build:
	(cd ../../grammar; $(MAKE))

.PHONY: build all
