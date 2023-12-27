EXAMPLES = hello_world fac tfac module_calls ackermannr
SAB_FILES = $(EXAMPLES:%=%.sab)
SA=../../bin/sa
SAC=../../bin/sac

all: $(EXAMPLES)

hello_world: build
	$(SAC) hello_world.sai
	$(SA) hello_world 0

fac: build
	$(SAC) fac.sai
	$(SA) fac 0 5

tfac: build
	$(SAC) tfac.sai
	$(SA) tfac 0 5

tfac: module_calls
	$(SAC) module_calls.sai
	$(SA) module_calls 0 5

ackermannr: build
	$(SAC) ackermannr.sai
	$(SA) ackermannr 0 3 2

build:
	(cd ../../src/sa; $(MAKE) all)
	(cd ../../src/sac; $(MAKE) all)

clean:
	rm -f $(SAB_FILES)
