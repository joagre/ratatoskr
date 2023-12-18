all: hello_world fac tfac ackermannr

SA=../../bin/sa

hello_world: build
	$(SA) hello_world 0

fac: build
	$(SA) fac 0 5

tfac: build
	$(SA) tfac 0 5

ackermannr: build
	$(SA) ackermannr 0 3 2

build:
	(cd ../../src/sa; $(MAKE) all)

clean:
