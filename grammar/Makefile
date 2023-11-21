CC = gcc
PCC = packcc
CFLAGS = -Wall -Wextra -std=c17 -g -pedantic
LDLIBS = -lm

all: satie satie.pdf

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

satie.pdf: ../ideas/satie.md
	pandoc --toc --pdf-engine=xelatex -f gfm -o $@ $^

Like this:


clean:
	rm -f satie satie.c satie.h satie.o satie.pdf *.d

.PHONY: all clean
