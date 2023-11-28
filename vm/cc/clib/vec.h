// vectors
#ifndef __VEC_H__

#include <stdint.h>
#include <memory.h>

// fixme: configure
#if __SIZEOF_FLOAT__ == 4
typedef float float32_t;
#endif
#if __SIZEOF_DOUBLE__ == 8
typedef double float64_t;
#endif

#define CAT_HELPER4(p,x,y,z) p ## x ## _ ## y ## _ ## z
#define CAT4(p,x,y,z) CAT_HELPER4(p,x,y,z)

#define CAT_HELPER3(p,x,y) p ## x ## _ ## y
#define CAT3(p,x,y) CAT_HELPER3(p,x,y)

#define CAT_HELPER2(x,y) x ## y
#define CAT2(x,y) CAT_HELPER2(x,y)

// unary operations
#define ULOOP(n, src, dst, op)		\
    while((n)--) {				\
	*(dst) = (op) (*(src));			\
	(src)++;				\
	(dst)++;				\
    }

// binay operations
#define BLOOP(n, src1, src2, dst, op) \
    while((n)--) {				\
	*(dst) = (op) (*(src1), *(src2));	\
	(src1)++;				\
	(src2)++;				\
	(dst)++;				\
    }

#define VEC_UFUNC(P,F,T)						\
VEC_LOCAL void CAT4(vec_,P,F,T)(vec_t* a, vec_t* c)			\
{									\
    int n = imin2(a->capacity, c->capacity);				\
    CAT3(vec_,T,t)* ap = (CAT3(vec_,T,t)*) a->base;			\
    CAT3(vec_,T,t)* cp = (CAT3(vec_,T,t)*) c->base;			\
    ULOOP(n,ap,cp,CAT3(vecop_,F,T));					\
}

#define VEC_UFUNC_TAB(P,F)		\
static vec_uop_t CAT4(vec_,P,F,ops)[] = {	\
    NULL,					\
    CAT4(vec_,P,F,int8),			\
    CAT4(vec_,P,F,int16),			\
    CAT4(vec_,P,F,int32),			\
    CAT4(vec_,P,F,int64),			\
    CAT4(vec_,P,F,float32),			\
    CAT4(vec_,P,F,float64)			\
};

#define VEC_BFUNC(P,F,T)						\
VEC_LOCAL void CAT4(vec_,P,F,T)(vec_t* a, vec_t* b, vec_t* c)	\
{									\
    int n = imin3(a->capacity, b->capacity, c->capacity);		\
    CAT3(vec_,T,t)* ap = (CAT3(vec_,T,t)*) a->base;			\
    CAT3(vec_,T,t)* bp = (CAT3(vec_,T,t)*) b->base;			\
    CAT3(vec_,T,t)* cp = (CAT3(vec_,T,t)*) c->base;			\
    BLOOP(n,ap,bp,cp,CAT3(vecop_,F,T));					\
}

#define VEC_BFUNC_TAB(P,F)		\
static vec_bop_t CAT4(vec_,P,F,ops)[] = {	\
    NULL,					\
    CAT4(vec_,P,F,int8),			\
    CAT4(vec_,P,F,int16),			\
    CAT4(vec_,P,F,int32),			\
    CAT4(vec_,P,F,int64),			\
    CAT4(vec_,P,F,float32),			\
    CAT4(vec_,P,F,float64)			\
};

#define VEC_CFUNC(P,F,T,D)					\
VEC_LOCAL void CAT4(vec_,P,F,T)(vec_t* a, vec_t* b, vec_t* c)	\
{									\
    int n = imin3(a->capacity, b->capacity, c->capacity);		\
    CAT3(vec_,T,t)* ap = (CAT3(vec_,T,t)*) a->base;			\
    CAT3(vec_,T,t)* bp = (CAT3(vec_,T,t)*) b->base;			\
    CAT3(vec_,D,t)* cp = (CAT3(vec_,D,t)*) c->base;			\
    BLOOP(n,ap,bp,cp,CAT3(vecop_,F,T));					\
}

#define VEC_CFUNC_TAB(P,F)				\
static vec_bop_t CAT4(vec_,P,F,ops)[] = {		\
    NULL,						\
    CAT4(vec_,P,F,int8),				\
    CAT4(vec_,P,F,int16),				\
    CAT4(vec_,P,F,int32),				\
    CAT4(vec_,P,F,int64),				\
    CAT4(vec_,P,F,float32),				\
    CAT4(vec_,P,F,float64)				\
};


#if defined(__x86__) || defined(__x86_64__)
#include <cpuid.h>
#endif

#if defined(__ARM_NEON__)
#include <arm_neon.h>
#endif

#include "allocator.h"

#define VEC_ADDR(vp,i) ((void*)((intptr_t)((vp)->base) + ((i)*(vp)->width)))
#define ALIGN_OFFS(p,a) (((a) - (((uintptr_t)p) % (a))) % (a))
#define ALIGN(p,a) ((void*)(((uint8_t*)p)+ALIGN_OFFS(p,a)))

#define VEC_LOCAL static
#if defined(__WIN32__) || defined(_WIN32)
#define VEC_API
#else
#define VEC_API __attribute__ ((unused))
#endif

typedef enum
{
    VEC_SIMD_NONE   = 0x01,    // native non-simd
    VEC_SIMD_MMX    = 0x02,    // 64 bit
    VEC_SIMD_SSE    = 0x04,    // 128 bit
    VEC_SIMD_AVX    = 0x08,    // 256 bit
    VEC_SIMD_AVX512 = 0x10,    // 512 bit
    VEC_SIMD_NEON   = 0x20,    // 128 bit (64-bit later?)
    VEC_SIMD_AUTO   = 0x00,    // select best available
} vec_simd_type_t;

typedef enum
{
    VEC_TYPE_BITS  = 0,
    VEC_TYPE_INT8  = 1,
    VEC_TYPE_INT16 = 2,
    VEC_TYPE_INT32 = 3,
    VEC_TYPE_INT64 = 4,
    VEC_TYPE_FLOAT32 = 5,
    VEC_TYPE_FLOAT64 = 6
} vec_type_t;

typedef struct
{
    uint32_t capacity; // number of simd vectors
    uint32_t size;     // number of elements used (< capacity*(VSIZE/width))
    uint32_t n;        // number of elements in n direction
    uint32_t m;        // number of elements in n direction
    uint8_t  width;    // element byte size 1,2,4,8
    uint8_t  wshift;   // width in shifts 0,1,2,3
    uint8_t  type;     // VEC_TYPE/BITS/INT8/INT16/INT32/FLOAT32/FLOAT64
    allocator_t* alloc;
    void*  base0;      // allocated memory (=NULL for static)
    void*  base;       // aligned memory
} vec_t;

typedef void (*vec_uop_t)(vec_t* src, vec_t* dst);
typedef void (*vec_bop_t)(vec_t* src1, vec_t* src2, vec_t* dst);

// vector operation tables
typedef struct
{
    char* name;        // name of simd type
    size_t size;       // byte size of vector
    size_t align;      // alignment needed

    intptr_t (*get_element_int)(vec_t* src, int i);
    double   (*get_element_float)(vec_t* src, int i);

    void (*set_element_int)(vec_t* src, int i, intptr_t value);
    void (*set_element_float)(vec_t* src, int i, double value);    
	      
    vec_uop_t* neg;
    vec_bop_t* add;
    vec_bop_t* sub;
    vec_bop_t* min;
    vec_bop_t* max;
    vec_bop_t* cmpeq;
    vec_bop_t* cmpgt;
    vec_bop_t* cmplt;
    vec_uop_t bnot;
    vec_bop_t band;
    vec_bop_t bor;
    vec_bop_t bxor;
} vec_alg_t;


typedef struct _vec_globals_t
{
    unsigned neon:1;
    unsigned mmx:1;    
    unsigned sse:1;
    unsigned sse2:1;
    unsigned sse3:1;
    unsigned sse4_1:1;
    unsigned sse4_2:1;
    unsigned avx:1;
    unsigned avx2:1;
    unsigned avxvnni:1;
    struct {
	unsigned f:1;	  // foundation
	unsigned cd:1;	  // conflict detection
	unsigned er:1;	  // exponential and reciprocal
	unsigned pf:1;	  // prefetch
	unsigned vl:1;    // vector lenth extension
	unsigned dq:1;    // doubleword and quadword
	unsigned bw:1;    // byte and word instructions
	unsigned ifma:1;  // fused multiply add
	unsigned vbmi:1;  // byte manipulation instructions
	unsigned _4vnniw:1; // vector neural network instruction
	unsigned _4fmaps:1; // fused multipy accuumation packed single precision
	unsigned vpopcntdq:1;  // bit count
	unsigned vnni:1;  // vector neural network instructions
	unsigned vbmi2:1; // vector byte manipulation instructions 2
	unsigned bitalg:1; // bit algorithms
	unsigned vp2intersect:1;  // vector pair intersection
	unsigned gfni:1;       // 
	unsigned vpclmulqdq:1;
	unsigned vaes:1;
	unsigned bf16:1;       // Float16 support
    } avx512;
    size_t max_vector_size;    
    unsigned cache_line_size;
    char cpu_vendor_name[14];
    vec_alg_t* algp;
} vec_globals_t;

VEC_LOCAL vec_globals_t vec_globals;


static inline int imin2(int a, int b)
{
    return (a < b) ? a : b;
}

static inline int imin3(int a, int b, int c)
{
    return imin2(a, imin2(b, c));
}

#define VEC_ALIGN (vec_globals.algp->align)
#define VEC_SIZE  (vec_globals.algp->size)

VEC_LOCAL inline size_t vec_elements_per_VEC(size_t width)
{
    return (VEC_SIZE == 1) ? 1 : (VEC_SIZE/width);
}

VEC_LOCAL int vec_init(vec_t* vp, allocator_t* alloc,
		       size_t initial_capacity, vec_type_t type,
		       size_t width) VEC_API;
VEC_LOCAL void vec_clear(vec_t* vp) VEC_API;
VEC_LOCAL size_t vec_capacity(vec_t* vp) VEC_API;
VEC_LOCAL size_t vec_size(vec_t* vp) VEC_API;
VEC_LOCAL int vec_set_capacity(vec_t* vp, size_t capacity) VEC_API;
VEC_LOCAL int vec_resize(vec_t* vp, size_t size) VEC_API;
VEC_LOCAL void* vec_element(vec_t* vp, int i) VEC_API;
VEC_LOCAL void vec_setelement(vec_t* vp,int i,void* data) VEC_API;

#if defined(__MMX__)
#include "vec_mmx.h"
#define VEC_SIMD_TYPE mmx
#define VEC_SIMD_NAME "mmx"
#define VEC_SIMD_ALG vec_alg_mmx
static vec_alg_t vec_alg_mmx;
#include "vec_ops.h"
#endif

#if !defined(NOSIMD) && defined(__SSE__)
#include "vec_sse.h"
#define VEC_SIMD_TYPE sse
#define VEC_SIMD_NAME "sse"
#define VEC_SIMD_ALG vec_alg_sse
static vec_alg_t vec_alg_sse;
#include "vec_ops.h"
#endif

#if !defined(NOSIMD) && defined(__AVX__)
#include "vec_avx.h"
#define VEC_SIMD_TYPE avx
#define VEC_SIMD_NAME "avx"
#define VEC_SIMD_ALG vec_alg_avx
static vec_alg_t vec_alg_avx;
#include "vec_ops.h"
#endif

// #if !defined(NOSIMD) && defined(__AVX512F__)
// #include "vec_avx512.h"
// #endif

#if !defined(NOSIMD) && defined(__ARM_NEON__)
#include "vec_neon.h"
#define VEC_SIMD_TYPE neon
#define VEC_SIMD_NAME "neon"
#define VEC_SIMD_ALG vec_alg_neon
static vec_alg_t vec_alg_neon;
#include "vec_ops.h"
#endif

// fallback using regular scalar types
#include "vec_scalar.h"
#define VEC_SIMD_TYPE scl
#define VEC_SIMD_NAME "scalar"
#define VEC_SIMD_ALG vec_alg_scl
static vec_alg_t vec_alg_scl;
#include "vec_ops.h"

VEC_LOCAL intptr_t vec_element_int(vec_t* vp, int i) VEC_API;
VEC_LOCAL double vec_element_float(vec_t* vp, int i) VEC_API;
VEC_LOCAL void vec_setelement_int(vec_t* vp,int i,intptr_t value) VEC_API;
VEC_LOCAL void vec_setelement_float(vec_t* vp,int i,double value) VEC_API;
VEC_LOCAL void vec_info(vec_t* a) VEC_API;
VEC_LOCAL void vec_print(vec_t* a) VEC_API;
// operation per vector implementation
VEC_LOCAL void vec_neg(vec_t* a, vec_t* c);
VEC_LOCAL void vec_add(vec_t* a, vec_t* b, vec_t* c);
VEC_LOCAL void vec_sub(vec_t* a, vec_t* b, vec_t* c);
VEC_LOCAL void vec_min(vec_t* a, vec_t* b, vec_t* c);
VEC_LOCAL void vec_max(vec_t* a, vec_t* b, vec_t* c);
VEC_LOCAL void vec_cmpeq(vec_t* a, vec_t* b, vec_t* c);
VEC_LOCAL void vec_cmpgt(vec_t* a, vec_t* b, vec_t* c);
VEC_LOCAL void vec_cmplt(vec_t* a, vec_t* b, vec_t* c);
VEC_LOCAL void vec_bnot(vec_t* a, vec_t* c);
VEC_LOCAL void vec_band(vec_t* a, vec_t* b, vec_t* c);
VEC_LOCAL void vec_bor(vec_t* a, vec_t* b, vec_t* c);
VEC_LOCAL void vec_bxor(vec_t* a, vec_t* b, vec_t* c);

#if defined(__x86__) || defined(__x86_64__)
// name must be at least 13 chars long
VEC_LOCAL char* cpuid_vendor_name(char* name)
{
    uint32_t a,b,c,d;
    __cpuid(0,a,b,c,d);
    *((uint32_t*)&name[0]) = b;
    *((uint32_t*)&name[4]) = d;
    *((uint32_t*)&name[8]) = c;
    name[12] = '\0';
    return name;
}

VEC_LOCAL int cpuid_feature(uint32_t* ecx, uint32_t* edx)
{
    uint32_t a, b, c, d;
    __cpuid(1,a,b,c,d);
    *ecx = c;
    *edx = d;
    return 0;
}

VEC_LOCAL int cpuid_get_highfunsup()
{
    return __get_cpuid_max(0, 0);
}

VEC_LOCAL int cpuid_amd_feature(uint32_t* ecx, uint32_t* edx)
{
    uint32_t a, b, c, d;
    __cpuid(0x80000001,a,b,c,d);
    *ecx = c;
    *edx = d;
    return 0;
}

// name must be at least 49 chars long 
VEC_LOCAL char* cpuid_amd_brand_string(char* name)
{
    uint32_t a,b,c,d;

    __cpuid(0x80000002,a,b,c,d);
    *((uint32_t*)&name[0])  = a;
    *((uint32_t*)&name[4])  = b;
    *((uint32_t*)&name[8])  = c;
    *((uint32_t*)&name[12]) = d;

    __cpuid(0x80000003,a,b,c,d);
    *((uint32_t*)&name[16]) = a;
    *((uint32_t*)&name[20]) = b;
    *((uint32_t*)&name[24]) = c;
    *((uint32_t*)&name[28]) = d;
    
    __cpuid(0x80000004,a,b,c,d);
    *((uint32_t*)&name[32]) = a;
    *((uint32_t*)&name[36]) = b;
    *((uint32_t*)&name[40]) = c;
    *((uint32_t*)&name[44]) = d;
    name[48] = '\0';
    return name;
}

VEC_LOCAL int cpuid_ext_feature(unsigned int sub,
				uint32_t* eax,uint32_t* ebx,
				uint32_t* ecx, uint32_t* edx)
{
    uint32_t info[4];
    __cpuidex(info,7,sub);
    *eax = info[0];
    *ebx = info[1];
    *ecx = info[2];
    *edx = info[3];
    return 0;
}

#define	CPUID_CLFUSH_SIZE	0x0000ff00
VEC_LOCAL int cpuid_cache_line_size()
{
    uint32_t a, b, c, d;
    __cpuid(1,a,b,c,d);
    return ((b & CPUID_CLFUSH_SIZE) >> 8) << 3;
}
#endif


VEC_LOCAL int vec_globals_init(vec_simd_type_t sel)
{
#if defined(__x86__) || defined(__x86_64__)
    uint32_t ecx = 0;
    uint32_t edx = 0;
    uint32_t ext0_eax = 0;
    uint32_t ext0_ebx = 0;
    uint32_t ext0_ecx = 0;
    uint32_t ext0_edx = 0;
    uint32_t ext1_eax = 0;
    uint32_t ext1_ebx = 0;
    uint32_t ext1_ecx = 0;
    uint32_t ext1_edx = 0;
    int maxlevel = 0;    
#endif
    vec_alg_t* algp = NULL;
    memset(&vec_globals, 0, sizeof(vec_globals));

#if defined(__x86__) || defined(__x86_64__)
    maxlevel = cpuid_get_highfunsup();
    if (maxlevel == 0)
	return -1;
    cpuid_vendor_name(vec_globals.cpu_vendor_name);
    vec_globals.cache_line_size = cpuid_cache_line_size();

    cpuid_feature(&ecx, &edx);
    cpuid_ext_feature(0, &ext0_eax, &ext0_ebx,
		      &ext0_ecx, &ext0_edx);
    cpuid_ext_feature(1, &ext1_eax, &ext1_ebx,
		      &ext1_ecx, &ext1_edx);
    if (edx & bit_MMX) {
	vec_globals.mmx = 1;
#if !defined(NOSIMD) && defined(__MMX__)
	vec_globals.max_vector_size = sizeof(__m64);
	if ((sel & VEC_SIMD_MMX) || (sel == VEC_SIMD_AUTO))
	    algp = &vec_alg_mmx;
#endif
    }
    if (edx & bit_SSE) {
	vec_globals.sse = 1;
#if !defined(NOSIMD) && defined(__SSE__)
	vec_globals.max_vector_size = sizeof(__m128);
#endif
    }
    if (edx & bit_SSE2) {     // fixme: we require SSE4_1 right now
	vec_globals.sse2 = 1;
#if !defined(NOSIMD) && defined(__SSE2__)	
	vec_globals.max_vector_size = sizeof(__m128);
	
#endif
    }
    if (ecx & bit_SSE3) {
	vec_globals.sse3 = 1;
#if !defined(NOSIMD) && defined(__SSE3__)		
	vec_globals.max_vector_size = sizeof(__m128);
#endif
    }
    if (ecx & bit_SSE4_1) {
	vec_globals.sse4_1 = 1;
#if !defined(NOSIMD) && defined(__SSE4_1__)
	vec_globals.max_vector_size = sizeof(__m128);
	if ((sel & VEC_SIMD_SSE) || (sel == VEC_SIMD_AUTO))
	    algp = &vec_alg_sse;
#endif
    }
    if (ecx & bit_SSE4_2) {
	vec_globals.sse4_2 = 1;
#if !defined(NOSIMD) && defined(__SSE4_2__)
	vec_globals.max_vector_size = sizeof(__m128);
#endif	
    }
    if (ecx & bit_AVX) {
	vec_globals.avx = 1;
#if !defined(NOSIMD) && defined(__AVX__)
	vec_globals.max_vector_size = sizeof(__m256);	
#endif
    }
    if (ext0_ebx & bit_AVX2) {
    	vec_globals.avx2 = 1;
#if !defined(NOSIMD) && defined(__AVX2__)	
	vec_globals.max_vector_size = sizeof(__m256);
	if ((sel & VEC_SIMD_AVX) || (sel == VEC_SIMD_AUTO))
	    algp = &vec_alg_avx;
#endif
    }
    // avx512 features
    if (ext0_ebx & bit_AVX512F) {
	vec_globals.avx512.f = 1;
#ifdef __AVX512F__ 
	// algp = &vec_alg_avx512;
	vec_globals.max_vector_size = sizeof(__m512);
	if ((sel & VEC_SIMD_AVX512) || (sel == VEC_SIMD_AUTO))
	    algp = &vec_alg_avx512;
#endif
    }
    if (ext0_ebx & bit_AVX512CD)
	vec_globals.avx512.cd = 1;
    if (ext0_ebx & bit_AVX512ER)
	vec_globals.avx512.er = 1;    
    if (ext0_ebx & bit_AVX512PF)
	vec_globals.avx512.pf = 1;    
    if (ext0_ebx & bit_AVX512VL)
	vec_globals.avx512.vl = 1;
    if (ext0_ebx & bit_AVX512DQ)
	vec_globals.avx512.dq = 1;
    if (ext0_ebx & bit_AVX512BW)
	vec_globals.avx512.bw = 1;
    if (ext0_ebx & bit_AVX512IFMA)
	vec_globals.avx512.ifma = 1;
    if (ext0_ecx & bit_AVX512VBMI)
	vec_globals.avx512.vbmi = 1;
    if (ext0_edx & bit_AVX5124VNNIW)
	vec_globals.avx512._4vnniw = 1;
    if (ext0_edx & bit_AVX5124FMAPS)
	vec_globals.avx512._4fmaps = 1;
    if (ext0_ecx & bit_AVX512VPOPCNTDQ)
	vec_globals.avx512.vpopcntdq = 1;
    if (ext0_ecx & bit_AVX512VNNI)
	vec_globals.avx512.vnni = 1;
    if (ext0_ecx & bit_AVX512VBMI2)
	vec_globals.avx512.vbmi2 = 1;
    if (ext0_ecx & bit_AVX512BITALG)
	vec_globals.avx512.bitalg = 1;    
    if (ext0_edx & bit_AVX512VP2INTERSECT)
	vec_globals.avx512.vp2intersect = 1;
    if (ext1_eax & bit_AVX512BF16)
	vec_globals.avx512.bf16 = 1;
    if (ext1_eax & bit_AVXVNNI)
	vec_globals.avxvnni = 1;
#elif defined(__ARM_NEON__)
    vec_globals.neon = 1;
    vec_globals.max_vector_size = sizeof(int8x16_t);
    if ((sel & VEC_SIMD_NEON) || (sel == VEC_SIMD_AUTO))
	algp = &vec_alg_neon;
#endif
    if ((algp == NULL) && ((sel & VEC_SIMD_NONE)||(sel == VEC_SIMD_AUTO)))
	algp = &vec_alg_scl;
    vec_globals.algp = algp;
    return 0;
}

VEC_LOCAL int vec_vinit(vec_t* vp, allocator_t* alloc,
			size_t size, size_t width, void* base)
{
    if ((size % VEC_SIZE) != 0)  // must be multiple
	return 0;
    if (ALIGN_OFFS(base, VEC_ALIGN) != 0) // must be aligne
	return 0;
    if (alloc == NULL) alloc = allocator_std();    
    vp->capacity = size/VEC_SIZE;  // number of vectors
    vp->size     = size;           // number of elements
    vp->width    = width;          // element size in bytes
    vp->alloc    = alloc;
    vp->base0    = NULL;
    vp->base     = base;
    return 0;
}

VEC_LOCAL int vec_init(vec_t* vp, allocator_t* alloc,
		       size_t initial_capacity,
		       vec_type_t type,
		       size_t width)
{
    void* base = NULL;
    void* base0 = NULL;
    size_t capacity;

    if (alloc == NULL) alloc = allocator_std();
    
    // capacity in number of vectors to allocate
    if (VEC_SIZE == 1) {
	capacity = initial_capacity;
	if (capacity != 0) {
	    base0 = allocator_alloc(alloc, capacity*width+sizeof(intptr_t)-1);
	    if (base0 == NULL)
		return -1;
	    base = base0 + ALIGN_OFFS(base0, sizeof(intptr_t));
	}
    }
    else {
	capacity = (initial_capacity*width+VEC_SIZE-1) / VEC_SIZE;
	if (capacity != 0) {
	    base0 = allocator_alloc(alloc, capacity*VEC_SIZE + VEC_SIZE - 1);
	    if (base0 == NULL)
		return -1;
	    base = base0 + ALIGN_OFFS(base0, VEC_ALIGN);
	}
    }
    vp->capacity = capacity;  // number of vectors
    vp->size     = 0;         // number of elements
    vp->n        = 0;         // number of elements
    vp->m        = 0;         // number of elements
    vp->type     = type;      // bits/integer/float
    vp->width    = width;     // element size in bytes
    switch(width) {
    case 1:  vp->wshift = 0; break;
    case 2:  vp->wshift = 1; break;
    case 4:  vp->wshift = 2; break;
    case 8:  vp->wshift = 3; break;
    case 16: vp->wshift = 4; break;
    }
    vp->alloc    = alloc;
    vp->base0    = base0;
    vp->base     = base;
    return 0;
}

VEC_LOCAL void vec_clear(vec_t* vp)
{
    allocator_free(vp->alloc, vp->base0);
    vp->base = NULL;    
    vp->base0 = NULL;
    vp->size = 0;
    vp->capacity = 0;
}


VEC_LOCAL inline size_t vec_elements_per_vector(vec_t* a)
{
    return vec_elements_per_VEC(a->width);
}

VEC_LOCAL inline size_t vec_capacity(vec_t* a)
{
    return a->capacity*vec_elements_per_vector(a);
}

VEC_LOCAL inline size_t vec_size(vec_t* a)
{
    return a->size;
}

VEC_LOCAL int vec_set_capacity(vec_t* a, size_t capacity)
{
    void* base0;
    void* base;

    if (VEC_SIZE > 1)
	capacity = (capacity*a->width+VEC_SIZE-1) / VEC_SIZE;
    if (capacity == 0)
	vec_clear(a);
    else {
	size_t nelems, nbytes, size, offs, align;
	if (VEC_SIZE == 1) {
	    align = sizeof(intptr_t);
	    nelems = capacity;
	    nbytes = capacity*a->width;
	}
	else {
	    align = VEC_ALIGN;
	    nelems = capacity*vec_elements_per_vector(a);
	    nbytes = capacity*VEC_SIZE;
	}
	size = (a->size < nelems) ? a->size : nelems;
	base0 = allocator_realloc(a->alloc, a->base0, nbytes+align-1);
	if (base0 == NULL)
	    return -1;	
	offs = ALIGN_OFFS(base0, align);
	base = base0 + offs;
	
	if (ALIGN_OFFS(a->base0, align) != offs)
	    memmove(base, a->base, size*a->width);	
	a->base0 = base0;
	a->base = base;
	a->capacity = capacity;
	a->size = size;	
    }
    return 0;	
}

VEC_LOCAL int vec_resize(vec_t* a, size_t size)
{
    size_t size0;
    size_t nelems = a->capacity*vec_elements_per_vector(a);    
    if (size > nelems) {
	if (vec_set_capacity(a, size) < 0)
	    return -1;
    }
    if ((size0 = a->size) < size) {
	void* ptr = (uint8_t*)a->base + a->size*a->width;
	memset(ptr, 0, (size-size0)*a->width);
    }
    a->size = size;
    return 0;
}

VEC_LOCAL void* vec_element(vec_t* a, int i)
{
    if ((i < 0) || (i >= (int)a->size)) return NULL;
    return VEC_ADDR(a, i);
}

// copy data into vec resize if needed
VEC_LOCAL void vec_setelement(vec_t* vp,int i,void* data)
{
    if (i < 0) return;
    if (i >= (int)vp->size) {
	if (vec_resize(vp, i+1) < 0)
	    return;
    }
    memcpy(VEC_ADDR(vp, i), data, vp->width);
}

static const char* vec_type_name[] = {
    [VEC_TYPE_BITS] = "bits",
    [VEC_TYPE_INT8]   = "int8_t",
    [VEC_TYPE_INT16]  = "int16_t",
    [VEC_TYPE_INT32]  = "int32_t",
    [VEC_TYPE_INT64]  = "int64_t",
    [VEC_TYPE_FLOAT32]  = "float",
    [VEC_TYPE_FLOAT64]  = "double"
};

VEC_LOCAL void vec_info(vec_t* a)
{
    printf("capacity: %d\n", a->capacity);
    printf("size: %d\n", a->size);
    printf("n: %d\n", a->n);
    printf("m: %d\n", a->m);
    printf("width: %d\n", a->width);
    printf("wshift: %d\n", a->wshift);
    printf("type: %s\n", vec_type_name[a->type]);
    printf("base0: %p\n", a->base0);
    printf("base: %p\n", a->base);
}

VEC_LOCAL void vec_print(vec_t* a)
{
    int i;
    vec_info(a);
    printf("|");
    switch(a->type) {
    case VEC_TYPE_FLOAT32:
    case VEC_TYPE_FLOAT64:	
	for (i = 0; i < a->size; i++) {
	    double v = vec_element_float(a, i);
	    printf(" %f", v);
	}
	break;
    case VEC_TYPE_INT8:
    case VEC_TYPE_INT16:
    case VEC_TYPE_INT32:
    case VEC_TYPE_INT64:	
	for (i = 0; i < a->size; i++) {
	    intptr_t v = vec_element_int(a, i);
	    printf(" %ld", v);
	}
	break;
    case VEC_TYPE_BITS:
	for (i = 0; i < a->size; i++) {
	    intptr_t v = vec_element_int(a, i);
	    printf(" %0*lx", a->width, v);
	}
	break;
    default:
	break;
    }
    printf("|\n");
}

VEC_LOCAL intptr_t vec_element_int(vec_t* a, int i)
{
    return (vec_globals.algp->get_element_int)(a, i);
}

VEC_LOCAL double vec_element_float(vec_t* a, int i)
{
    return (vec_globals.algp->get_element_float)(a, i);
}

VEC_LOCAL void vec_setelement_int(vec_t* a, int i, intptr_t v)
{
    (vec_globals.algp->set_element_int)(a, i, v);
}

VEC_LOCAL void vec_setelement_float(vec_t* a, int i, double v)
{
    (vec_globals.algp->set_element_float)(a, i, v);
}

VEC_LOCAL void vec_neg(vec_t* a, vec_t* c)
{
    (vec_globals.algp->neg[a->type])(a, c);
}

VEC_LOCAL void vec_add(vec_t* a, vec_t* b, vec_t* c)
{
    (vec_globals.algp->add[a->type])(a, b, c);
}

VEC_LOCAL void vec_sub(vec_t* a, vec_t* b, vec_t* c)
{
    (vec_globals.algp->sub[a->type])(a, b, c);
}

VEC_LOCAL void vec_min(vec_t* a, vec_t* b, vec_t* c)
{
    (vec_globals.algp->min[a->type])(a, b, c);
}

VEC_LOCAL void vec_max(vec_t* a, vec_t* b, vec_t* c)
{
    (vec_globals.algp->max[a->type])(a, b, c);
}

VEC_LOCAL void vec_cmpeq(vec_t* a, vec_t* b, vec_t* c)
{
    (vec_globals.algp->cmpeq[a->type])(a, b, c);
}

VEC_LOCAL void vec_cmpgt(vec_t* a, vec_t* b, vec_t* c)
{
    (vec_globals.algp->cmpgt[a->type])(a, b, c);
}

VEC_LOCAL void vec_cmplt(vec_t* a, vec_t* b, vec_t* c)
{
    (vec_globals.algp->cmplt[a->type])(a, b, c);
}

VEC_LOCAL void vec_bnot(vec_t* a, vec_t* c)
{
    (vec_globals.algp->bnot)(a, c);
}

VEC_LOCAL void vec_band(vec_t* a, vec_t* b, vec_t* c)
{
    (vec_globals.algp->band)(a, b, c);
}

VEC_LOCAL void vec_bor(vec_t* a, vec_t* b, vec_t* c)
{
    (vec_globals.algp->bor)(a, b, c);
}

VEC_LOCAL void vec_bxor(vec_t* a, vec_t* b, vec_t* c)
{
    (vec_globals.algp->bxor)(a, b, c);
}

#endif
