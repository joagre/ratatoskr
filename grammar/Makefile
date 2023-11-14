CC = gcc
PCC = packcc
CFLAGS = -Wall -Wextra -O3 -std=c17 -g -pedantic -march=native -ffast-math
LDLIBS = -lm

all: satie

satie: satie.o
	$(CC) $(LDLIBS) -o $@ $^

satie.c: satie.peg
	$(PCC) $^

satie.o: satie.c
	$(CC) -c $(CFLAGS) -Wno-unused-parameter $< -o $@ -MMD -MF $(@:.o=.d)

test: satie
	./satie < test.sa

todo: satie
	./satie < todo.sa

ackermann: satie
	./satie < ackermann.sa

%.o: %.c
	$(CC) -c $(CFLAGS) $< -o $@ -MMD -MF $(@:.o=.d)

clean:
	rm -f satie satie.c satie.h satie.o *.d

.PHONY: all clean
