CC = gcc
PCC = packcc
CFLAGS = -Wall -Wextra -I../clib -I../utils -std=gnu17 -g -D_POSIX_C_SOURCE=200809L -Wno-unused-parameter -DDEBUG #-DUNITTEST
# Find all .c and .h files, exclude satie.c for dependency list
CFILES := $(filter-out satie.c, $(wildcard *.c))
HFILES := $(wildcard *.h)
# Combine .c and .h files for full dependency list for satie.c
SATIE_DEPS := $(CFILES) $(HFILES)
SRCS := $(wildcard *.c)
# Ensure satie.c is not automatically included in SRCS
SRCS := $(filter-out satie.c, $(SRCS))
OBJS := $(SRCS:.c=.o)
EXTRA_OBJS := ../utils/log.o \
              ../utils/satie_error.o
DEPS := $(SRCS:.c=.d)
EXEC := ../../bin/lint

-include $(DEPS)

all: build $(EXEC) no_actions_satie.peg

build:
	cd ../utils && $(MAKE) objs

objs: $(OBJS) satie.o

$(EXEC): $(OBJS) $(EXTRA_OBJS) satie.o
	$(CC) $(CFLAGS) -o $@ $^

satie.c: satie.peg $(SATIE_DEPS)
	$(PCC) satie.peg

%.o: %.c
	$(CC) $(CFLAGS) -MMD -MP -c $< -o $@

no_actions_satie.peg: satie.peg
	cat $^ | ./remove_actions.pl > $@

clean:
	rm -f $(OBJS) $(DEPS) $(EXEC) satie.h satie.c no_actions_satie.peg
