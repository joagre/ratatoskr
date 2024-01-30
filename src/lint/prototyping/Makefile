ERLC=$(shell which erlc)
ERLC_FLAGS=+debug_info
ERL_SOURCES=$(wildcard *.erl)
ERL_OBJECTS=$(ERL_SOURCES:%.erl=%.beam)

all: $(ERL_OBJECTS)

clean:
	rm -f $(ERL_OBJECTS)

%.beam: %.erl
	$(ERLC) $(ERLC_FLAGS) $<
