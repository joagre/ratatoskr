all: ackermann0 ackermann10 ackermann1 ackermannr fac hello_world message_passing module_calls tfac

ackermann0: build
	../bin/sa ackermann 0

ackermann10: build
	../bin/sa ackermann 10

ackermann1: build
	../bin/sa ackermann 1 3 6

ackermannr: build
	../bin/sa -i register ackermannr 10

fac: build
	../bin/sa fac 0 10

hello_world: build
	../bin/sa hello_world 0

message_passing: build
	../bin/sa message_passing 0 7

module_calls: build
	../bin/sa module_calls 0 10

tfac: build
	../bin/sa tfac 0 10 1

build:
	(cd ..; $(MAKE))

clean:

.PHONY: all ackermann0 ackermann10 ackermann1 ackermannr fac hello_world message_passing module_calls tfac build clean
