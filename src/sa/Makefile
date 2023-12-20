CC := gcc
CFLAGS := -Wall -Wextra -I../clib -I../utils -I../sac -Wfatal-errors -std=c17 -g -D_GNU_SOURCE -Wno-unused-value -DDEBUG # -DUNITTEST #-O3  #-pg #-O3 # -pg # -DDEBUG #
LDFLAGS := ../utils/satie_error.o ../utils/utils.o ../utils/log.o

SRCS := $(wildcard *.c)
OBJS := $(SRCS:.c=.o)
DEPS := $(SRCS:.c=.d)
EXEC := ../../bin/sa

all: build $(EXEC)

build:
	cd ../utils && $(MAKE) objs
	cd ../sac && $(MAKE) objs

objs: $(OBJS)

$(EXEC): $(OBJS)
	$(CC) $(CFLAGS) $(LDFLAGS) -o $@ $^

-include $(DEPS)

%.o: %.c
	$(CC) $(CFLAGS) -MMD -MP -c $< -o $@

clean:
	rm -f $(OBJS) $(DEPS) $(EXEC)
