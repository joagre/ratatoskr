POSM_FILES=hello_world.posm fac.posm tfac.posm module_calls.posm ackermann.posm

build: $(POSM_FILES)

%.posm: posm-spec.txt
	awk "/__$*__start__/{flag=1;next}/__$*__end__/{flag=0}flag" $< > $@

clean:
	rm -f $(POSM_FILES)
