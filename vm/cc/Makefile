CC := gcc
CFLAGS := -Wall -Wextra -std=c17 -g -pedantic
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
