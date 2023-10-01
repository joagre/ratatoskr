all: build

build:
	(cd doc; make build)
	(cd erlang; make build)
	(cd d; make build)

d-ex1:
	(cd d; make build)
	./d/bin/posm doc/example1.posm 0 10000000 10

d-ex2:
	(cd d; make build)
	./d/bin/posm doc/example2.posm 0 10000000 10

erl-ex1:
	(cd erlang; make build)
	env ERL_LIBS=erlang erl -run posm start doc/example1.posm 0

erl-ex2:
	(cd erlang; make build)
	env ERL_LIBS=erlang erl -run posm start doc/example2.posm 0

clean:
	(cd doc; make clean)
	(cd erlang; make clean)
	(cd d; make clean)
