CC := gcc
CFLAGS := -Wall -Wextra -Wfatal-errors -std=c17 -g -DDEBUG -DUNITTEST -D_GNU_SOURCE \
-Wno-sign-compare \
-Wno-unused-value \
-Wno-unused-function
SRCS := $(wildcard *.c)
OBJS := $(SRCS:.c=.o)
DEPS := $(SRCS:.c=.d)
EXEC := sa

all: $(EXEC)

$(EXEC): $(OBJS)
	$(CC) $(CFLAGS) -o $@ $^

-include $(DEPS)

%.o: %.c
	$(CC) $(CFLAGS) -MMD -MP -c $< -o $@

clean:
	rm -f $(OBJS) $(DEPS) $(EXEC)

mrproper: clean
	find . \( -name "*~" -or -name '#*' -or -name '.#*' \) -exec rm {} \;;

.PHONY: all clean
