all:
	(cd src; $(MAKE) all)

clean:
	(cd src; $(MAKE) clean)

.PHONY: all clean

mrproper: clean
	find . \( -name "*~" -or -name '#*' -or -name '.#*' \) -exec rm {} \;;
