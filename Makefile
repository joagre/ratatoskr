all:
	(cd src/prototype; $(MAKE) all)

clean:
	(cd src/prototype; $(MAKE) clean)

.PHONY: all clean

mrproper: clean
	find . \( -name "*~" -or -name '#*' -or -name '.#*' \) -exec rm {} \;;
