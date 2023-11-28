// AVX vectors
#ifndef __VEC_AVX_H__

#include <stdint.h>
#include <memory.h>

#if defined(__AVX__)
//#warning including AVX
#include <immintrin.h>

#include "vec_undef.h"

#define vec_native_t __m256
#define vec_int_t    __m256i
#define vec_flt_t    __m256
#define vec_dbl_t    __m256d
// vector access types
#define vec_int8_t     vec_int_t
#define vec_int16_t    vec_int_t
#define vec_int32_t    vec_int_t
#define vec_int64_t    vec_int_t
#define vec_float32_t    vec_flt_t
#define vec_float64_t    vec_dbl_t
// element access types
#define elem_int8_t    __v32qs
#define elem_int16_t   __v16hi
#define elem_int32_t   __v8si
#define elem_int64_t   __v4di
#define elem_float32_t   __v8sf
#define elem_float64_t   __v4df

#define vecop_neg_int8      MM256_neg_epi8
#define vecop_neg_int16     MM256_neg_epi16
#define vecop_neg_int32     MM256_neg_epi32
#define vecop_neg_int64     MM256_neg_epi64
#define vecop_neg_float32     MM256_neg_ps
#define vecop_neg_float64     MM256_neg_pd

#define vecop_add_int8      _mm256_add_epi8
#define vecop_add_int16     _mm256_add_epi16
#define vecop_add_int32     _mm256_add_epi32
#define vecop_add_int64     _mm256_add_epi64
#define vecop_add_float32     _mm256_add_ps
#define vecop_add_float64     _mm256_add_pd

#define vecop_sub_int8      _mm256_sub_epi8
#define vecop_sub_int16     _mm256_sub_epi16
#define vecop_sub_int32     _mm256_sub_epi32
#define vecop_sub_int64     _mm256_sub_epi64
#define vecop_sub_float32     _mm256_sub_ps
#define vecop_sub_float64     _mm256_sub_pd

#define vecop_min_int8      _mm256_min_epi8
#define vecop_min_int16     _mm256_min_epi16
#define vecop_min_int32     _mm256_min_epi32
#define vecop_min_int64     MM256_min_epi64
#define vecop_min_float32     _mm256_min_ps
#define vecop_min_float64     _mm256_min_pd

#define vecop_max_int8      _mm256_max_epi8
#define vecop_max_int16     _mm256_max_epi16
#define vecop_max_int32     _mm256_max_epi32
#define vecop_max_int64     MM256_max_epi64
#define vecop_max_float32     _mm256_max_ps
#define vecop_max_float64     _mm256_max_pd

#define vecop_cmpgt_int8      MM256_cmpgt_epi8
#define vecop_cmpgt_int16     MM256_cmpgt_epi16
#define vecop_cmpgt_int32     MM256_cmpgt_epi32
#define vecop_cmpgt_int64     MM256_cmpgt_epi64
#define vecop_cmpgt_float32   MM256_cmpgt_ps
#define vecop_cmpgt_float64   MM256_cmpgt_pd

#define vecop_cmplt_int8      MM256_cmplt_epi8
#define vecop_cmplt_int16     MM256_cmplt_epi16
#define vecop_cmplt_int32     MM256_cmplt_epi32
#define vecop_cmplt_int64     MM256_cmplt_epi64
#define vecop_cmplt_float32   MM256_cmplt_ps
#define vecop_cmplt_float64   MM256_cmplt_pd

#define vecop_band_bits       _mm256_and_si256
#define vecop_bor_bits        _mm256_or_si256
#define vecop_bxor_bits       _mm256_xor_si256
#define vecop_bnot_bits       MM256_bnot_si256

#if defined(__AVX2__)
#include <immintrin.h>
#endif

static __m256i MM256_cmplt_epi8(__m256i a, __m256i b)
{
    return (__m256i) ((__v32qs)a < (__v32qs)b);
}

static __m256i MM256_cmplt_epi16(__m256i a, __m256i b)
{
    return (__m256i) ((__v16hi)a < (__v16hi)b);
}

static __m256i MM256_cmplt_epi32(__m256i a, __m256i b)
{
    return (__m256i) ((__v8si)a < (__v8si)b);
}

static __m256i MM256_cmplt_epi64(__m256i a, __m256i b)
{
    return (__m256i) ((__v4di)a < (__v4di)b);
}

static __m256i MM256_cmplt_ps(__m256 a, __m256 b)
{
    return (__m256i) ((__v8sf)a < (__v8sf)b);
}

static __m256i MM256_cmplt_pd(__m256d a, __m256d b)
{
    return (__m256i) ((__v4df)a < (__v4df)b);
}

static __m256i MM256_cmpgt_epi8(__m256i a, __m256i b)
{
    return (__m256i) ((__v32qs)a > (__v32qs)b);
}

static __m256i MM256_cmpgt_epi16(__m256i a, __m256i b)
{
    return (__m256i) ((__v16hi)a > (__v16hi)b);
}

static __m256i MM256_cmpgt_epi32(__m256i a, __m256i b)
{
    return (__m256i) ((__v8si)a > (__v8si)b);
}

static __m256i MM256_cmpgt_epi64(__m256i a, __m256i b)
{
    return (__m256i) ((__v4di)a > (__v4di)b);
}

static __m256i MM256_cmpgt_ps(__m256 a, __m256 b)
{
    return (__m256i) ((__v8sf)a > (__v8sf)b);
}

static __m256i MM256_cmpgt_pd(__m256d a, __m256d b)
{
    return (__m256i) ((__v4df)a > (__v4df)b);
}

static __m256i MM256_bnot_si256(__m256i a)
{
    return (__m256i) (~(__v4di)a);
}

static __m256i MM256_neg_epi8(__m256i a)
{
    return (__m256i) (-(__v32qs)a);
}

static __m256i MM256_neg_epi16(__m256i a)
{
    return (__m256i) (-(__v16hi)a);
}

static __m256i MM256_neg_epi32(__m256i a)
{
    return (__m256i) (-(__v8si)a);
}

static __m256i MM256_neg_epi64(__m256i a)
{
    return (__m256i) (-(__v4di)a);
}

static __m256 MM256_neg_ps(__m256 a)
{
    return (__m256) (-(__v8sf)a);
}

static __m256d MM256_neg_pd(__m256d a)
{
    return (__m256d) (-(__v4df)a);
}

static __m256i MM256_min_epi64(__m256i a, __m256i b)
{
    __m256i m = (__m256i) ((__v4di)a < (__v4di)b);
    return (m & a) | (~m & b);    
}

static __m256i MM256_max_epi64(__m256i a, __m256i b)
{
    __m256i m = (__m256i) ((__v4di)a > (__v4di)b);
    return (m & a) | (~m & b);
}


#endif  // __AVX__
#endif  // __VEC_AVX_H__
