READMESRCS = tribute.sa color.sa ackermann.sa todo.sa
READMEOBJS = $(READMESRCS:%=../examples/sa/%) ../src/lint/satie.peg

SAISRCS = hello_world.sai fac.sai tfac.sai module_calls.sai ackermann.sai message_passing.sai
SAIOBJS = $(SAISRCS:%=../examples/sai/%)

all: ../README.md satie.pdf ../SAI.md

../README.md: README.md.in $(READMEOBJS)
	cp README.md.in ../README.md
	for obj in $(READMEOBJS); do \
	    sed -i '/__'`basename $$obj`'__/r '$$obj ../README.md; \
	    sed -i '/__'`basename $$obj`'__/d' ../README.md; \
        done

satie.pdf: ../README.md
	(cd ..; pandoc --toc --pdf-engine=xelatex -f gfm -o doc/$@ README.md)

../SAI.md: SAI.md.in $(SAIOBJS)
	cp SAI.md.in ../SAI.md
	for obj in $(SAIOBJS); do \
	    sed -i '/__'`basename $$obj`'__/r '$$obj ../SAI.md; \
	    sed -i '/__'`basename $$obj`'__/d' ../SAI.md; \
        done

clean:
	rm -f satie.pdf
