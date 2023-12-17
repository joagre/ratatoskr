all: build

build:
#	dub build --build=unittest --debug=interpreter --debug=scheduler --debug=user
#	dub build --build=unittest --debug=user --debug=scheduler
#	dub build --build=release --debug=user
	dub build --build=debug --debug=user
#	dub build --build=release --compiler=ldc2 --debug=user
#	dub build --build=profile

clean:
	dub clean
	rm -f bin/sa

examples: ackermann0 ackermann10 ackermann1 ackermannr fac hello_world message_passing module_calls tfac

ackermann0: build
	bin/sa -l ../examples ackermann 0

ackermann10: build
	bin/sa -l ../examples ackermann 10

ackermann1: build
	bin/sa -l ../examples ackermann 1 3 6

ackermannr: build
	bin/sa -l ../examples -i register ackermannr 10

fac: build
	bin/sa -l ../examples fac 0 10

hello_world: build
	bin/sa -l ../examples hello_world 0

message_passing: build
	bin/sa -l ../examples message_passing 0 7

module_calls: build
	bin/sa -l ../examples module_calls 0 10

tfac: build
	bin/sa -l ../examples tfac 0 10 1

.PHONY: all clean ackermann0 ackermann10 ackermann1 ackermannr fac hello_world message_passing module_calls tfac
