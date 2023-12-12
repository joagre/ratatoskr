CC = gcc
PCC = packcc
CFLAGS = -Wall -Wextra -std=c17 -g -D_POSIX_C_SOURCE=200809L -Wno-unused-parameter
LDLIBS = -lm

all: lint clean_satie.peg

lint: satie.o
	$(CC) $(LDLIBS) -o $@ $^

clean_satie.peg: satie.peg
	cat satie.peg | remove_actions.pl > clean_satie.peg

examples:
	(cd examples; $(MAKE))

satie.c: satie.peg
	$(PCC) $^

%.o: %.c
	$(CC) $(CFLAGS) -c $< -o $@

clean:
	rm -f lint satie.o satie.h satie.c

.PHONY: all clean examples
