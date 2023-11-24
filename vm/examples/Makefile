all: ackermann0 ackermann10 ackermann1 ackermannr fac hello_world \
message_passing module_calls tfac

ackermann0: build
	../../d/bin/r ackermann 0

ackermann10: examplesbuild dbuild
	../../d/bin/r ackermann 10

ackermann1: examplesbuild dbuild
	../../d/bin/r ackermann 1 3 6

ackermannr: build
	../../d/bin/r -i register ackermannr 10

fac: examplesbuild dbuild
	./d/bin/r -l ./examples fac 0 10

hello_world: examplesbuild
	./d/bin/r -l ./examples hello_world 0

message_passing: examplesbuild dbuild
	./d/bin/r -l ./examples message_passing 0 7

module_calls: examplesbuild dbuild
	./d/bin/r -l ./examples module_calls 0 10

tfac: examplesbuild dbuild
	./d/bin/r -l ./examples tfac 0 10 1

build:
	(cd ../../d; $(MAKE))

.PHONY: all build ackermann0 ackermann10 ackermann1 ackermannr fac hello_world \
message_passing module_calls tfac
