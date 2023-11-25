FRAGMENTS = ../grammar/examples/tribute.sa ../grammar/examples/color.sa ../grammar/examples/ackermann.sa ../grammar/examples/todo.sa ../grammar/satie.peg

all: ../README.md satie.pdf

satie.pdf: ../README.md
	(cd ..; pandoc --toc --pdf-engine=xelatex -f gfm -o doc/$@ README.md)

../README.md: README.md.in $(FRAGMENTS)
	cp README.md.in ../README.md
	for frag in $(FRAGMENTS); do \
	    sed -i '/__'`basename $$frag`'__/r '$$frag ../README.md; \
	    sed -i '/__'`basename $$frag`'__/d' ../README.md; \
        done

clean:
	rm -f satie.pdf

.PHONY: all clean
