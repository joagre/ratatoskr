CC = gcc
PCC = packcc
CFLAGS = -Wall -Wextra -I../clib -std=c17 -g -D_POSIX_C_SOURCE=200809L -Wno-unused-parameter
LDLIBS = -lm

all: ../../bin/sac no_actions_satie.peg

../../bin/sac: satie.o
	$(CC) $(LDLIBS) -o $@ $^

no_actions_satie.peg: satie.peg
	cat $^ | ./remove_actions.pl > $@

satie.c: satie.peg satie_auxil.h
	$(PCC) satie.peg

%.o: %.c
	$(CC) $(CFLAGS) -c $< -o $@

clean:
	rm -f ../../bin/sac no_actions_satie.peg satie.o satie.h satie.c

.PHONY: all clean examples
