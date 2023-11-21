all: satie.pdf

satie.pdf: ../README.md
	pandoc --toc --pdf-engine=xelatex -f gfm -o $@ $^

clean:
	rm -f satie.pdf

.PHONY: all clean
