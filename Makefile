all:
	(cd vm; $(MAKE) all)

clean:
	(cd vm; $(MAKE) clean)

.PHONY: all clean

mrproper: clean
	find . \( -name "*~" -or -name '#*' -or -name '.#*' \) -exec rm {} \;;
