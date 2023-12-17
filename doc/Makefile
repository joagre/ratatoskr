SRCS = tribute.sa color.sa ackermann.sa todo.sa
OBJS = $(SRCS:%=../examples/%) ../src/sac/satie.peg

all: ../README.md satie.pdf

satie.pdf: ../README.md
	(cd ..; pandoc --toc --pdf-engine=xelatex -f gfm -o doc/$@ README.md)

../README.md: README.md.in $(OBJS)
	cp README.md.in ../README.md
	for obj in $(OBJS); do \
	    sed -i '/__'`basename $$obj`'__/r '$$obj ../README.md; \
	    sed -i '/__'`basename $$obj`'__/d' ../README.md; \
        done

clean:
	rm -f satie.pdf

.PHONY: all clean
