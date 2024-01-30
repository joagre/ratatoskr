CC := gcc
CFLAGS := -Wall -Wextra -I../clib -I../utils -I../sac -Wfatal-errors -std=gnu17 -g -D_GNU_SOURCE -Wno-unused-value -DDEBUG #-DUNITTEST #-O3  #-pg #-O3 # -pg # -DDEBUG #
SRCS := $(wildcard *.c)
OBJS := $(SRCS:.c=.o)
EXTRA_OBJS := ../utils/log.o \
              ../utils/satie_error.o \
              ../utils/utils.o
DEPS := $(SRCS:.c=.d)
EXEC := ../../bin/sa

all: build $(EXEC)

build:
	cd ../utils && $(MAKE) objs
	cd ../sac && $(MAKE) objs

objs: $(OBJS)

$(EXEC): $(OBJS) $(EXTRA_OBJS)
	$(CC) $(CFLAGS) -o $@ $^

-include $(DEPS)

%.o: %.c
	$(CC) $(CFLAGS) -MMD -MP -c $< -o $@

clean:
	rm -f $(OBJS) $(DEPS) $(EXEC)
