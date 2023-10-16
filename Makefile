all: build
build: dbuild examplesbuild

dbuild:
	(cd d; make build)

examplesbuild:
	(cd examples; make build)

examples: hello_world fac tfac module_calls ackermann0 ackermann10 ackermann1 message_passing

hello_world: examplesbuild
	./d/bin/r -l ./examples hello_world 0

fac: examplesbuild dbuild
	./d/bin/r -l ./examples fac 0 10

tfac: examplesbuild dbuild
	./d/bin/r -l ./examples tfac 0 10 1

module_calls: examplesbuild dbuild
	./d/bin/r -l ./examples module_calls 0 10

ackermann0: examplesbuild dbuild
	./d/bin/r -l ./examples ackermann 0

ackermann10: examplesbuild dbuild
	./d/bin/r -l ./examples ackermann 10

ackermann1: examplesbuild dbuild
	./d/bin/r -l ./examples ackermann 1 3 6

message_passing: examplesbuild dbuild
	./d/bin/r -l ./examples message_passing 0 7

clean:
	(cd examples; make clean)
	(cd d; make clean)

mrproper: clean
	find . \( -name erl_crash.dump -or -name '*.beam' -or -name "*~" -or -name '#*' -or -name '.#*' \) -exec rm {} \;;
