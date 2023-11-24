CC = gcc
PCC = packcc
CFLAGS = -Wall -Wextra -std=c17 -g -pedantic
LDLIBS = -lm

all: satie

satie: satie.o
	$(CC) $(LDLIBS) -o $@ $^

satie.c: satie.peg
	$(PCC) $^

satie.o: satie.c
	$(CC) -c $(CFLAGS) -Wno-unused-parameter $< -o $@ -MMD -MF $(@:.o=.d)

## Examples

all_examples: test todo ackermann color tribute

test: satie
	./satie < test.sa

todo: satie
	./satie < todo.sa

ackermann: satie
	./satie < ackermann.sa

color: satie
	./satie < color.sa

tribute: satie
	./satie < tribute.sa

clean:
	rm -f satie satie.c satie.h satie.o *.d

.PHONY: all clean
