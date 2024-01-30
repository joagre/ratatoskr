CC := gcc
CFLAGS := -Wall -Wextra -I../clib -Wfatal-errors -std=gnu17 -g -D_GNU_SOURCE -DDEBUG

SRCS := $(wildcard *.c)
OBJS := $(SRCS:.c=.o)
DEPS := $(SRCS:.c=.d)

all: objs

objs: $(OBJS)

-include $(DEPS)

%.o: %.c
	$(CC) $(CFLAGS) -MMD -MP -c $< -o $@

clean:
	rm -f $(OBJS) $(DEPS)
