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

CFLAGS = -MMD -MP -MF .$<.d 
CFLAGS += -g -Wall -Wextra


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

DEMOS = \
	lhash_demo \
	lhash_kv_demo \
	lhash_kv_demo_int \
	lhash_kv_demo_dyn

all: $(TESTS) $(DEMOS)

lhash_test: lhash_test.o
	$(CC) -o$@ $(CFLAGS) lhash_test.o

avl_test: avl_test.o
	$(CC) -o$@ $(CFLAGS) avl_test.o

dlist_test: CFLAGS += -DDLIST
dlist_test:	list_test.o
	$(CC) -o$@ $(CFLAGS) list_test.o

cdlist_test: CFLAGS += -DCDLIST
cdlist_test:	list_test.o
	$(CC) -o$@ $(CFLAGS) list_test.o

slist_test: CFLAGS += -DSLIST
slist_test:	list_test.o
	$(CC) -o$@ $(CFLAGS) list_test.o

dynarray_test: CFLAGS += -DDYNARR
dynarray_test:	arr_test.o
	$(CC) -o$@ $(CFLAGS) arr_test.o

segarray_test: CFLAGS += -DSEGARR
segarray_test:	arr_test.o
	$(CC) -o$@ $(CFLAGS) arr_test.o

atom_test: atom_test.o
	$(CC) -o$@ $(CFLAGS) atom_test.o

pred_test: pred_test.o 
	$(CC) -o$@ $(CFLAGS) pred_test.o

bignum_test: bignum_test.o
	$(CC) -o$@ $(CFLAGS) bignum_test.o

##vec_test_mmx: vec_test.c vec.h
##	$(CC) -o$@  -O1 -mmmx -mno-sse  $(CFLAGS) vec_test.c

vec_test_sse: CFLAGS += -msse4.1
vec_test_sse: vec_test.o
	$(CC) -o$@  -O1 $(CFLAGS) vec_test.o

vec_test_avx: CFLAGS += -mavx2
vec_test_avx: vec_test.o
	$(CC) -o$@  -O1 $(CFLAGS) vec_test.o

vec_test_neon: CFLAGS += -mfpu=neon -mfloat-abi=hard -flax-vector-conversions
vec_test_neon: vec_test.o
	$(CC) -o$@  -O1 $(CFLAGS) vec_test.o

vec_test_scalar: CFLAGS += -DNOSIMD
vec_test_scalar: vec_test.o
	$(CC) -o$@ -O1 $(CFLAGS) vec_test.o

## Demo programs

lhash_demo: lhash_demo.o
	$(CC) -o$@ $(CFLAGS) lhash_demo.o

lhash_kv_demo: lhash_kv_demo.o
	$(CC) -o$@ $(CFLAGS) lhash_kv_demo.o

lhash_kv_demo_int: lhash_kv_demo_int.o
	$(CC) -o$@ $(CFLAGS) lhash_kv_demo_int.o

lhash_kv_demo_dyn: lhash_kv_demo_dyn.o
	$(CC) -o$@ $(CFLAGS) lhash_kv_demo_dyn.o


%.o:	%.c
	$(CC) $(CFLAGS) -c $<

clean:
	rm $(TESTS)

.%.d:	;

-include .*.d
