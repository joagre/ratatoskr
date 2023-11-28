// SSE vectors
#ifndef __VEC_SSE_H__

#include <stdint.h>
#include <memory.h>

#if defined(__SSE__)
// #warning including SSE
#include <xmmintrin.h>

#if defined(__SSE2__)
#include <emmintrin.h>
#endif

#if defined(__SSE3__)
#include <pmmintrin.h>
#endif

#if defined(__SSE4_1__)
#include <smmintrin.h>
#endif


#include "vec_undef.h"

#define vec_native_t __m128
#define vec_int_t    __m128i
#define vec_flt_t    __m128
#define vec_dbl_t    __m128d
// vector access types
#define vec_int8_t     vec_int_t
#define vec_int16_t    vec_int_t
#define vec_int32_t    vec_int_t
#define vec_int64_t    vec_int_t
#define vec_float32_t  vec_flt_t
#define vec_float64_t  vec_dbl_t
// element access types
#define elem_int8_t    __v16qs
#define elem_int16_t   __v8hi
#define elem_int32_t   __v4si
#define elem_int64_t   __v2di
#define elem_float32_t   __v4sf
#define elem_float64_t   __v2df

#define vecop_neg_int8      MM128_neg_epi8
#define vecop_neg_int16     MM128_neg_epi16
#define vecop_neg_int32     MM128_neg_epi32
#define vecop_neg_int64     MM128_neg_epi64
#define vecop_neg_float32   MM128_neg_ps
#define vecop_neg_float64   MM128_neg_pd

#define vecop_add_int8      _mm_add_epi8
#define vecop_add_int16     _mm_add_epi16
#define vecop_add_int32     _mm_add_epi32
#define vecop_add_int64     _mm_add_epi64
#define vecop_add_float32     _mm_add_ps
#define vecop_add_float64     _mm_add_pd

#define vecop_sub_int8      _mm_sub_epi8
#define vecop_sub_int16     _mm_sub_epi16
#define vecop_sub_int32     _mm_sub_epi32
#define vecop_sub_int64     _mm_sub_epi64
#define vecop_sub_float32     _mm_sub_ps
#define vecop_sub_float64     _mm_sub_pd

#define vecop_min_int8      _mm_min_epi8
#define vecop_min_int16     _mm_min_epi16
#define vecop_min_int32     _mm_min_epi32
#define vecop_min_int64     MM128_min_epi64
#define vecop_min_float32     _mm_min_ps
#define vecop_min_float64     _mm_min_pd

#define vecop_max_int8      _mm_max_epi8
#define vecop_max_int16     _mm_max_epi16
#define vecop_max_int32     _mm_max_epi32
#define vecop_max_int64     MM128_max_epi64
#define vecop_max_float32   _mm_max_ps
#define vecop_max_float64   _mm_max_pd

#define vecop_band_bits     _mm_and_si128
#define vecop_bor_bits      _mm_or_si128
#define vecop_bxor_bits     _mm_xor_si128
#define vecop_bnot_bits     MM128_bnot_si128

#if defined(__SSE2__)
//#warning including SSE2
#include <emmintrin.h>
#define vecop_cmpgt_int8      _mm_cmpgt_epi8
#define vecop_cmpgt_int16     _mm_cmpgt_epi16
#define vecop_cmpgt_int32     _mm_cmpgt_epi32
#define vecop_cmpgt_int64     MM128_cmpgt_epi64
#define vecop_cmpgt_float32   MM128_cmpgt_ps
#define vecop_cmpgt_float64   MM128_cmpgt_pd

#define vecop_cmplt_int8      _mm_cmplt_epi8
#define vecop_cmplt_int16     _mm_cmplt_epi16
#define vecop_cmplt_int32     _mm_cmplt_epi32
#define vecop_cmplt_int64     MM128_cmplt_epi64
#define vecop_cmplt_float32   MM128_cmplt_ps
#define vecop_cmplt_float64   MM128_cmplt_pd
#endif

#if defined(__SSE3__)
//#warning including SSE3
#include <pmmintrin.h>
#endif

#if defined(__SSSE3__)
//#warning including SSSE3
#include <tmmintrin.h>
#endif

#if defined(__SSE4_1__)
//#warning including SSE4_1
#include <smmintrin.h>
#endif

static __m128i MM128_cmpgt_epi64(__m128i a, __m128i b)
{
    return (__m128i) ((__v2di)a > (__v2di)b);
}

static __m128i MM128_cmpgt_ps(__m128 a, __m128 b)
{
    return (__m128i) ((__v4sf)a > (__v4sf)b);
}

static __m128i MM128_cmpgt_pd(__m128d a, __m128d b)
{
    return (__m128i) ((__v2df)a > (__v2df)b);
}

static __m128i MM128_cmplt_epi64(__m128i a, __m128i b)
{
    return (__m128i) ((__v2di)a < (__v2di)b);
}

static __m128i MM128_cmplt_ps(__m128 a, __m128 b)
{
    return (__m128i) ((__v4sf)a < (__v4sf)b);
}

static __m128i MM128_cmplt_pd(__m128d a, __m128d b)
{
    return (__m128i) ((__v2df)a < (__v2df)b);
}


static __m128i MM128_min_epi64(__m128i a, __m128i b)
{
    __m128i m = (__m128i) ((__v2di)a < (__v2di)b);
    return (m & a) | (~m & b);    
}

static __m128i MM128_max_epi64(__m128i a, __m128i b)
{
    __m128i m = (__m128i) ((__v2di)a > (__v2di)b);
    return (m & a) | (~m & b);
}



static __m128i MM128_bnot_si128(__m128i a)
{
    return (__m128i) (~(__v2di)a);
}

static __m128i MM128_neg_epi8(__m128i a)
{
    return (__m128i) (-(__v16qs)a);
}

static __m128i MM128_neg_epi16(__m128i a)
{
    return (__m128i) (-(__v8hi)a);
}

static __m128i MM128_neg_epi32(__m128i a)
{
    return (__m128i) (-(__v4si)a);
}

static __m128i MM128_neg_epi64(__m128i a)
{
    return (__m128i) (-(__v2di)a);
}

static __m128 MM128_neg_ps(__m128 a)
{
    return (__m128) (-(__v4sf)a);
}

static __m128d MM128_neg_pd(__m128d a)
{
    return (__m128d) (-(__v2df)a);
}



#endif // __SSE__
#endif // __VEC_SSE_H__
