CC = gcc
PCC = packcc
CFLAGS = -Wall -Wextra -I../clib -I../utils -std=c17 -g -D_POSIX_C_SOURCE=200809L -Wno-unused-parameter -DDEBUG -DUNITTEST
SRCS := $(wildcard *.c)
OBJS := $(SRCS:.c=.o)
EXTRA_OBJS := ../utils/log.o \
              ../utils/satie_error.o
DEPS := $(SRCS:.c=.d)
EXEC := ../../bin/lint

all: build $(EXEC) no_actions_satie.peg

build:
	cd ../utils && $(MAKE) objs

objs: $(OBJS)

$(EXEC): $(OBJS) $(EXTRA_OBJS)
	$(CC) $(CFLAGS) -o $@ $^

satie.c: satie.peg satie_auxil.h symbol_table.h ast.h hm.h
	$(PCC) satie.peg

%.o: %.c
	$(CC) $(CFLAGS) -c $< -o $@

no_actions_satie.peg: satie.peg
	cat $^ | ./remove_actions.pl > $@

clean:
	rm -f $(OBJS) $(DEPS) $(EXEC) satie.h satie.c no_actions_satie.peg
