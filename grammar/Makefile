CC = gcc
PCC = packcc
CFLAGS = -Wall -Wextra -std=c17 -g -D_POSIX_C_SOURCE=200809L -Wno-unused-parameter
LDLIBS = -lm

all: lint satie_no_actions.peg

lint: satie.o
	$(CC) $(LDLIBS) -o $@ $^

satie_no_actions.peg: satie.peg
	echo cat $^ | ./remove_actions.pl > $@

examples:
	(cd examples; $(MAKE))

satie.c: satie.peg
	$(PCC) $^

%.o: %.c
	$(CC) $(CFLAGS) -c $< -o $@

clean:
	rm -f lint satie.o satie.h satie.c

.PHONY: all clean examples
