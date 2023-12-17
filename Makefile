all:
	(cd src; $(MAKE) all)
	(cd doc; $(MAKE) all)

clean:
	(cd src; $(MAKE) clean)
	(cd doc; $(MAKE) clean)

.PHONY: all clean

mrproper: clean
	find . \( -name "*~" -or -name '#*' -or -name '.#*' \) -exec rm {} \;;
