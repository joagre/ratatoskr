APP=$(shell basename `cd ..; pwd`)
ERLC=$(shell which erlc)
ERLC_FLAGS=+debug_info
ERL_SOURCES=$(wildcard *.erl)
ERL_OBJECTS=$(ERL_SOURCES:%.erl=../ebin/%.beam)

all: $(ERL_OBJECTS)

clean:
	rm -f $(ERL_OBJECTS)

../ebin/%.beam: %.erl
	$(ERLC) $(ERLC_FLAGS) -o ../ebin $<
