CC := gcc
CFLAGS := -Wall -Wextra -I../clib -I../utils -I ../sa -Wfatal-errors -std=c17 -g -D_GNU_SOURCE -Wno-unused-variable -DDEBUG
SRCS := $(wildcard *.c)
OBJS := $(SRCS:.c=.o)
EXTRA_OBJS := ../utils/log.o \
              ../sa/module.o \
              ../sa/pretty_print.o \
              ../utils/satie_error.o \
              ../sa/static_data.o \
	      ../utils/utils.o \
	      ../sa/vm.o
DEPS := $(SRCS:.c=.d)
EXEC := ../../bin/sac

all: build $(EXEC)

build:
	cd ../utils && $(MAKE) objs
	cd ../sa && $(MAKE) objs

objs: $(OBJS)

$(EXEC): $(OBJS) $(EXTRA_OBJS)
	$(CC) $(CFLAGS) -o $@ $^

-include $(DEPS)

%.o: %.c
	$(CC) $(CFLAGS) -MMD -MP -c $< -o $@

clean:
	rm -f $(OBJS) $(DEPS) $(EXEC)
