all: ackermann0 ackermann10 ackermann1 ackermannr fac hello_world \
message_passing module_calls tfac

ackermann0: build
	../d/bin/sa ackermann 0

ackermann10: build
	../d/bin/sa ackermann 10

ackermann1: build
	../d/bin/sa ackermann 1 3 6

ackermannr: build
	../d/bin/sa -i register ackermannr 10

fac: build
	../d/bin/sa fac 0 10

hello_world: build
	../d/bin/sa hello_world 0

message_passing: build
	../d/bin/sa message_passing 0 7

module_calls: build
	../d/bin/sa module_calls 0 10

tfac: build
	../d/bin/sa tfac 0 10 1

build:
	(cd ../d; $(MAKE))

.PHONY: all build ackermann0 ackermann10 ackermann1 ackermannr fac hello_world \
message_passing module_calls tfac
