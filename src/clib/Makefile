# Make all tests (with gdb flag!)
OSNAME := $(shell uname -s)
MACHINE := $(shell uname -m)
RELEASE := $(shell uname -r)

ifeq ($(MACHINE), i686)
TESTS_SIMD = vec_test_sse vec_test_scalar
endif

ifeq ($(MACHINE), x86_64)
TESTS_SIMD = vec_test_sse vec_test_avx vec_test_scalar 
endif

ifeq ($(MACHINE), armv7l)
TESTS_SIMD = vec_test_neon vec_test_scalar
endif

CFLAGS = -g -Wall -Wextra

TESTS = \
	lhash_test \
	avl_test \
	slist_test \
	dlist_test \
	cdlist_test \
	dynarray_test \
	segarray_test \
	atom_test \
	pred_test \
	bignum_test \
	$(TESTS_SIMD)

all: $(TESTS)

lhash_test: lhash_test.c lhash_kv.h lhash.h slist.h
	$(CC) -o$@ $(CFLAGS) lhash_test.c

avl_test: avl_test.c avl_kv.h avl.h allocator.h
	$(CC) -o$@ $(CFLAGS) avl_test.c

dlist_test:	list_test.c dlist.h
	$(CC) -o$@ $(CFLAGS) -DDLIST list_test.c

cdlist_test:	list_test.c cdlist.h
	$(CC) -o$@ $(CFLAGS) -DCDLIST list_test.c

slist_test:	list_test.c dlist.h
	$(CC) -o$@ $(CFLAGS) -DSLIST list_test.c

dynarray_test:	arr_test.c dynarr.h
	$(CC) -o$@ $(CFLAGS) -DDYNARR arr_test.c

segarray_test:	arr_test.c segarr.h
	$(CC) -o$@ $(CFLAGS) -DSEGARR arr_test.c

atom_test: atom_test.c atom.h lhash.h slist.h
	$(CC) -o$@ $(CFLAGS) atom_test.c

pred_test: pred_test.c atom.h pred.h pred_format.h pred_parse.h
	$(CC) -o$@ $(CFLAGS) pred_test.c

bignum_test: bignum_test.c bignum.h
	$(CC) -o$@ $(CFLAGS) bignum_test.c

##vec_test_mmx: vec_test.c vec.h
##	$(CC) -o$@  -O1 -mmmx -mno-sse  $(CFLAGS) vec_test.c

vec_test_sse: vec_test.c vec.h
	$(CC) -o$@  -O1 -msse4.1 $(CFLAGS) vec_test.c

vec_test_avx: vec_test.c vec.h
	$(CC) -o$@  -O1 -mavx2 $(CFLAGS) vec_test.c

vec_test_neon: vec_test.c vec.h
	$(CC) -o$@  -O1 -mfpu=neon -mfloat-abi=hard -flax-vector-conversions $(CFLAGS) vec_test.c

vec_test_scalar: vec_test.c vec.h
	$(CC) -o$@ -DNOSIMD -O1 $(CFLAGS) vec_test.c

clean:
	rm $(TESTS)
