all: build
build: dbuild erlbuild docbuild

dbuild:
	(cd d; make build)

erlbuild:
	(cd doc; make build)

docbuild:
	(cd erlang; make build)

d-example1: docbuild dbuild
	./d/bin/posm doc/example1.posm 0 10000000 10 1

d-example2: docbuild dbuild
	(cd d; make build)
	./d/bin/posm doc/example2.posm 0 10000000 10 100

erl-example1: docbuild erlbuild
	env ERL_LIBS=erlang erl -noinput -run posm start doc/example1.posm 0

erl-example2: docbuild erlbuild
	env ERL_LIBS=erlang erl -noinput -run posm start doc/example2.posm 0

clean:
	(cd doc; make clean)
	(cd erlang; make clean)
	(cd d; make clean)

mrproper: clean
	find . \( -name erl_crash.dump -or -name '*.beam' -or -name "*~" -or -name '#*' -or -name '.#*' \) -exec rm {} \;;
