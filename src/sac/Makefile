CC := gcc
CFLAGS := -Wall -Wextra -I../clib -I../utils -I ../sa -Wfatal-errors -std=c17 -g -D_GNU_SOURCE -Wno-unused-variable -DDEBUG
LDFLAGS := ../utils/satie_error.o ../utils/utils.o ../utils/log.o ../sa/vm.o

SRCS := $(wildcard *.c)
OBJS := $(SRCS:.c=.o)
DEPS := $(SRCS:.c=.d)
EXEC := ../../bin/sac

all: build $(EXEC)

build:
	cd ../utils && $(MAKE) build
	cd ../sa && $(MAKE) build

$(EXEC): $(OBJS)
	$(CC) $(CFLAGS) $(LDFLAGS) -o $@ $^

-include $(DEPS)

%.o: %.c
	$(CC) $(CFLAGS) -MMD -MP -c $< -o $@

clean:
	rm -f $(OBJS) $(DEPS) $(EXEC)
