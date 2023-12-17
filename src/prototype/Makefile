all: build
	$(MAKE) -f Makefile.README all
build:
#	dub build --build=unittest --debug=interpreter --debug=scheduler --debug=user
#	dub build --build=unittest --debug=user --debug=scheduler
#	dub build --build=release --debug=user
	dub build --build=debug --debug=user
#	dub build --build=release --compiler=ldc2 --debug=user
#	dub build --build=profile

examples:
	(cd examples; $(MAKE) -f Makefile)

clean:
	dub clean
	rm -f bin/sa
	$(MAKE) -f Makefile.README clean
	(cd examples; $(MAKE) -f Makefile clean)

.PHONY: all examples clean
