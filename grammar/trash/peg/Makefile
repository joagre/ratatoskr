CC = gcc
PCC = packcc
CFLAGS = -Wall -Wextra -std=c17 -g -pedantic -march=native -D_POSIX_C_SOURCE=200809L
LDFLAGS =
LDLIBS =
SRCS = example.c ast.c
OBJS = $(SRCS:.c=.o)
DEPS = $(SRCS:.c=.d)

all: example

-include $(DEPS)

example: example.o ast.o
	$(CC) $(LDFLAGS) $(LDLIBS) example.o ast.o -o $@

example.o: example.c
	$(CC) -c $(CFLAGS) -Wno-unused-parameter $< -o $@ -MMD -MF $(@:.o=.d)

example.c: example.peg
	$(PCC) $^

%.o: %.c
	$(CC) -c $(CFLAGS) $< -o $@ -MMD -MF $(@:.o=.d)

clean:
	rm -f $(DEPS) *.o example

.PHONY: all clean
