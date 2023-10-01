all: build

build:
	(cd doc; make build)
	(cd d; make build)

d-example1: build
	./d/bin/posm doc/example1.posm 0 10000000 10

d-example2: build
	./d/bin/posm doc/example2.posm 0 10000000 10

clean:
	(cd doc; make clean)
	(cd d; make clean)
