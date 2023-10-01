all: build

build:
	(cd doc; make build)
	(cd d; make build)

clean:
	(cd doc; make clean)
	(cd d; make clean)
