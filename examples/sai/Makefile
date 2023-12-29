EXAMPLES = hello_world fac tfac mcall ackermann message_passing mspawn
SAB_FILES = $(EXAMPLES:%=%.sab)
SA=../../bin/sa
SAC=../../bin/sac

all: $(EXAMPLES)

hello_world: build
	@echo "==== Example: $@"
	$(SAC) $@.sai
	$(SA) $@ 0

fac: build
	@echo "==== Example: $@"
	$(SAC) $@.sai
	$(SA) $@ 10 5

tfac: build
	@echo "==== Example: $@"
	$(SAC) $@.sai
	$(SA) $@ 10 5

mcall: build
	@echo "==== Example: $@"
	$(SAC) $@.sai
	$(SA) $@ 0 5

ackermann: build
	@echo "==== Example: $@"
	$(SAC) $@.sai
	$(SA) $@ 10 3 6

message_passing: build
	@echo "==== Example: $@"
	$(SAC) $@.sai
	$(SA) $@ 0 6

mspawn: build
	@echo "==== Example: $@"
	$(SAC) $@.sai
	$(SA) $@ 0 3 6

build:
	(cd ../../src/sa; $(MAKE) all)
	(cd ../../src/sac; $(MAKE) all)

clean:
	rm -f $(SAB_FILES)
