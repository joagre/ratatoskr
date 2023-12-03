CC := gcc
CFLAGS := -Wall -Werror -Wextra -std=c17 -g -DDEBUG -DUNITTEST -D_GNU_SOURCE -Wno-sign-compare -Wno-unused-function -Wno-unused-but-set-variable -Wno-unused-variable

SRCS := $(wildcard *.c)
OBJS := $(SRCS:.c=.o)
DEPS := $(SRCS:.c=.d)
EXEC := satie

all: $(EXEC)

$(EXEC): $(OBJS)
	$(CC) $(CFLAGS) -o $@ $^

-include $(DEPS)

%.o: %.c
	$(CC) $(CFLAGS) -MMD -MP -c $< -o $@

clean:
	rm -f $(OBJS) $(DEPS) $(EXEC)

.PHONY: all clean
