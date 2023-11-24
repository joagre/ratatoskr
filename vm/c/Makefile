CC = gcc
PCC = packcc
CFLAGS = -Wall -Wextra -O3 -std=c17 -g -pedantic -march=native -ffast-math
LDLIBS = -lm
#SRCS = $(wildcard *.c)
#OBJS = $(SRCS:.c=.o)

SC_SRCS = satie.c ast.c
SC_OBJS = $(SC_SRCS:.c=.o)
DEPS = $(SC_SRCS:.c=.d)

all: sc

-include $(DEPS)

#s: $(OBJS)
#	$(CC) -o $@ $^ $(LDLIBS)

sc: $(SC_OBJS)
	$(CC) -o $@ $^ $(LDLIBS)

satie.o: satie.c
	$(CC) -c $(CFLAGS) -Wno-unused-parameter $< -o $@ -MMD -MF $(@:.o=.d)

satie.c: satie.peg
	$(PCC) $^

%.o: %.c
	$(CC) -c $(CFLAGS) $< -o $@ -MMD -MF $(@:.o=.d)

clean:
	rm -f $(SC_OBJS) $(DEPS) r

.PHONY: all clean
