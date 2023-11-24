CC = gcc
PCC = packcc
CFLAGS = -Wall -Wextra -std=c17 -g -pedantic -Wno-unused-parameter
LDLIBS = -lm

lint: satie.o
	$(CC) $(LDLIBS) -o $@ $^

examples:
	(cd examples; $(MAKE))

satie.c: satie.peg
	$(PCC) $^

%.o: %.c
	$(CC) $(CFLAGS) -c $< -o $@

clean:
	rm -f lint satie.o satie.h satie.c

.PHONY: all clean examples
