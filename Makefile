all:
	(cd examples; $(MAKE) all)
	(cd doc; $(MAKE) all)
	(cd src; $(MAKE) all)

clean:
	(cd examples; $(MAKE) clean)
	(cd doc; $(MAKE) clean)
	(cd src; $(MAKE) clean)

mrproper: clean
	find . \( -name "*~" -or -name '#*' -or -name '.#*' \) -exec rm {} \;;
