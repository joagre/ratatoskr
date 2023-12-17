//
#include <stdio.h>
#include "vec.h"

#define PRINT_FEATURE(name) printf(#name "=%d\n", vec_globals.name)
#define PRINT_FEATURE_AVX512(name) printf("avx512." #name "=%d\n", vec_globals.avx512.name)

void test()
{
    vec_globals_init(VEC_SIMD_AUTO);

    printf("vendor: %s\n", vec_globals.cpu_vendor_name);
    printf("cache-line-size: %d\n", vec_globals.cache_line_size);

    PRINT_FEATURE(mmx);
    PRINT_FEATURE(sse);    
    PRINT_FEATURE(sse2);
    PRINT_FEATURE(sse3);
    PRINT_FEATURE(sse4_1);
    PRINT_FEATURE(sse4_2);
    PRINT_FEATURE(avx);
    PRINT_FEATURE(avx2);
    PRINT_FEATURE_AVX512(f);
    if (vec_globals.avx512.f) {
	PRINT_FEATURE_AVX512(cd);
	PRINT_FEATURE_AVX512(er);
	PRINT_FEATURE_AVX512(pf);
	PRINT_FEATURE_AVX512(vl);
	PRINT_FEATURE_AVX512(dq);
	PRINT_FEATURE_AVX512(bw);
	PRINT_FEATURE_AVX512(ifma);
	PRINT_FEATURE_AVX512(vbmi);
	PRINT_FEATURE_AVX512(_4vnniw);
	PRINT_FEATURE_AVX512(_4fmaps);
	PRINT_FEATURE_AVX512(vpopcntdq);
	PRINT_FEATURE_AVX512(vnni);
	PRINT_FEATURE_AVX512(vbmi2);
	PRINT_FEATURE_AVX512(bitalg);
	PRINT_FEATURE_AVX512(vp2intersect);
	PRINT_FEATURE_AVX512(gfni);
	PRINT_FEATURE_AVX512(vpclmulqdq);
	PRINT_FEATURE_AVX512(vaes);
    }
    printf("simd_type= %s\n", vec_globals.algp->name);
    printf("vector_size=%ld\n", vec_globals.algp->size);
    printf("vector_align=%ld\n", vec_globals.algp->align);
    printf("max_vector_size=%ld\n", vec_globals.max_vector_size);
}

void test_fop(vec_type_t type, int width)
{
    vec_t e, f, g;
    int i;
    
    vec_init(&e, 0, 19, type, width);
    vec_init(&f, 0, 30, type, width);
    vec_init(&g, 0, 19, type, width);
    vec_resize(&g, 19);

    for (i = 0; i < 19; i++) {
	vec_setelement_float(&e, i, ((double)i)+0.5);
    }
    printf("------ WIDTH = %d -----\n", width);
    printf("E=\n"); vec_print(&e); printf("\n");
    for (i = 0; i < 30; i++) {
	vec_setelement_float(&f, i, ((double)30-i)+0.25);
    }
    printf("F=\n"); vec_print(&f); printf("\n");

    vec_add(&e, &f, &g);
    printf("G=E+F\n"); vec_print(&g); printf("\n");

    vec_min(&e, &f, &g);
    printf("G=MIN(E,F)\n"); vec_print(&g); printf("\n");

    vec_max(&e, &f, &g);
    printf("G=MAX(A,B)\n"); vec_print(&g); printf("\n");        
    
}

void test_iop(vec_type_t type, int width)
{
    vec_t a, b, c;
    int i;
    
    vec_init(&a, 0, 19, type, width);
    vec_init(&b, 0, 30, type, width);
    vec_init(&c, 0, 19, type, width);
    vec_resize(&c, 19);

    for (i = 0; i < 19; i++) {
	vec_setelement_int(&a, i, i);
    }
    printf("------ WIDTH = %d -----\n", width);
    printf("A=\n"); vec_print(&a); printf("\n");

    for (i = 0; i < 30; i++) {
	vec_setelement_int(&b, i, 30-i);
    }
    printf("B=\n"); vec_print(&b); printf("\n");

    vec_add(&a, &b, &c);
    printf("C=A+B\n"); vec_print(&c); printf("\n");

    vec_sub(&a, &b, &c);
    printf("C=A-B\n"); vec_print(&c); printf("\n");

    vec_min(&a, &b, &c);
    printf("C=MIN(A,B)\n"); vec_print(&c); printf("\n");

    vec_max(&a, &b, &c);
    printf("C=MAX(A,B)\n"); vec_print(&c); printf("\n");        

    vec_bor(&a, &b, &c);
    printf("C=A|B\n"); vec_print(&c); printf("\n");

    vec_band(&a, &b, &c);
    printf("C=A&B\n"); vec_print(&c); printf("\n");

    vec_bxor(&a, &b, &c);
    printf("C=A^B\n"); vec_print(&c); printf("\n");

    vec_cmpgt(&a, &b, &c);
    printf("C=A>B\n"); vec_print(&c); printf("\n");

    vec_cmplt(&a, &b, &c);
    printf("C=A<B\n"); vec_print(&c); printf("\n");

    vec_cmpeq(&a, &b, &c);
    printf("C=A==B\n"); vec_print(&c); printf("\n");    
}


int main(int argc, char ** argv)
{
    (void) argc;
    (void) argv;
    
    test();
    test_iop(VEC_TYPE_INT8, sizeof(uint8_t));
    test_iop(VEC_TYPE_INT16, sizeof(uint16_t));
    test_iop(VEC_TYPE_INT32, sizeof(uint32_t));
    test_iop(VEC_TYPE_INT64, sizeof(uint64_t));

    test_fop(VEC_TYPE_FLOAT32, sizeof(float));
    test_fop(VEC_TYPE_FLOAT64, sizeof(double));
    exit(0);
}
