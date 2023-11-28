// MMX vectors
#ifndef __VEC_MMX_H__

#include <stdint.h>
#include <memory.h>

#if defined(__MMX__)
// #warning including MMX
#include <mmintrin.h>

#include "vec_undef.h"

#define vec_native_t __m64
#define vec_int_t    __m64
#define vec_flt_t    __m64
#define vec_dbl_t    __m64

// vector access types
#define vec_int8_t     vec_int_t
#define vec_int16_t    vec_int_t
#define vec_int32_t    vec_int_t
#define vec_int64_t    vec_int_t
#define vec_float32_t  vec_flt_t
#define vec_float64_t  vec_dbl_t
// element access types
#define elem_int8_t    __v8qi
#define elem_int16_t   __v4hi
#define elem_int32_t   __v2si
#define elem_int64_t   __v1di
#define elem_float32_t   __v2sf
#define elem_float64_t   __v2sf

#define vecop_neg_int8      MM64_neg_pi8
#define vecop_neg_int16     MM64_neg_pi16
#define vecop_neg_int32     MM64_neg_pi32
#define vecop_neg_int64     MM64_neg_si64
#define vecop_neg_float32   MM64_na1
#define vecop_neg_float64   MM64_na1

#define vecop_add_int8      _mm_add_pi8
#define vecop_add_int16     _mm_add_pi16
#define vecop_add_int32     _mm_add_pi32
#define vecop_add_int64     _mm_add_si64
#define vecop_add_float32   MM64_na2
#define vecop_add_float64   MM64_na2

#define vecop_sub_int8      _mm_sub_pi8
#define vecop_sub_int16     _mm_sub_pi16
#define vecop_sub_int32     _mm_sub_pi32
#define vecop_sub_int64     _mm_sub_si64
#define vecop_sub_float32   MM64_na2
#define vecop_sub_float64   MM64_na2

#define vecop_min_int8      MM64_min_pi8
#define vecop_min_int16     MM64_min_pi16
#define vecop_min_int32     MM64_min_pi32
#define vecop_min_int64     MM64_min_si64
#define vecop_min_float32   MM64_na2
#define vecop_min_float64   MM64_na2

#define vecop_max_int8      MM64_max_pi8
#define vecop_max_int16     MM64_max_pi16
#define vecop_max_int32     MM64_max_pi32
#define vecop_max_int64     MM64_max_si64
#define vecop_max_float32   MM64_na2
#define vecop_max_float64   MM64_na2

#define vecop_band_bits     _mm_and_si64
#define vecop_bor_bits      _mm_or_si64
#define vecop_bxor_bits     _mm_xor_si64
#define vecop_bnot_bits     MM64_bnot_si64

#define vecop_cmpeq_int8    _mm_cmpeq_pi8
#define vecop_cmpeq_int16   _mm_cmpeq_pi16
#define vecop_cmpeq_int32   _mm_cmpeq_pi32
#define vecop_cmpeq_int64   MM64_cmpeq_si64
#define vecop_cmpeq_float32 MM64_na2
#define vecop_cmpeq_float64 MM64_na2

#define vecop_cmpgt_int8    _mm_cmpgt_pi8
#define vecop_cmpgt_int16   _mm_cmpgt_pi16
#define vecop_cmpgt_int32   _mm_cmpgt_pi32
#define vecop_cmpgt_int64   MM64_cmpgt_si64
#define vecop_cmpgt_float32 MM64_na2
#define vecop_cmpgt_float64 MM64_na2

#define vecop_cmplt_int8    MM64_cmplt_pi8
#define vecop_cmplt_int16   MM64_cmplt_pi16
#define vecop_cmplt_int32   MM64_cmplt_pi32
#define vecop_cmplt_int64   MM64_cmplt_si64
#define vecop_cmplt_float32 MM64_na2
#define vecop_cmplt_float64 MM64_na2

static __m64 MM64_na1(__m64 a)
{
    return _mm_setzero_si64();
}

static __m64 MM64_na2(__m64 a, __m64 b)
{
    return _mm_setzero_si64();
}

static __m64 MM64_neg_pi8(__m64 a)
{
    return _mm_sub_pi8(_mm_setzero_si64(), a);
}

static __m64 MM64_neg_pi16(__m64 a)
{
    return _mm_sub_pi16(_mm_setzero_si64(), a);
}

static __m64 MM64_neg_pi32(__m64 a)
{
    return _mm_sub_pi32(_mm_setzero_si64(), a);
}

static __m64 MM64_neg_si64(__m64 a)
{
    return _mm_sub_si64(_mm_setzero_si64(), a);
}

static __m64 MM64_bnot_si64(__m64 a)
{
    return (__m64) (~(__v1di)a);
}

static __m64 MM64_cmpgt_si64(__m64 a, __m64 b)
{
    return (__m64) (((__v1di)a) > ((__v1di)b));
}

static __m64 MM64_cmplt_pi8(__m64 a, __m64 b)
{
    return _mm_cmpgt_pi8(b, a);
}

static __m64 MM64_cmplt_pi16(__m64 a, __m64 b)
{
    return _mm_cmpgt_pi16(b, a);
}

static __m64 MM64_cmplt_pi32(__m64 a, __m64 b)
{
    return _mm_cmpgt_pi32(b, a);
}

static __m64 MM64_cmplt_si64(__m64 a, __m64 b)
{
    return MM64_cmpgt_si64(b, a);
}

static __m64 MM64_cmpeq_si64(__m64 a, __m64 b)
{
    return (__m64) (((__v1di)a) == ((__v1di)b));
}


static __m64 MM64_max_pi8(__m64 a, __m64 b)
{
    __m64 m = (__m64) _mm_cmpgt_pi8(a, b);
    return _mm_or_si64(_mm_and_si64(m, a),
		       _mm_andnot_si64(m,b));
}

static __m64 MM64_max_pi16(__m64 a, __m64 b)
{
    __m64 m = (__m64) _mm_cmpgt_pi16(a, b);
    return _mm_or_si64(_mm_and_si64(m, a),
		       _mm_andnot_si64(m,b));
}

static __m64 MM64_max_pi32(__m64 a, __m64 b)
{
    __m64 m = (__m64) _mm_cmpgt_pi32(a, b);
    return _mm_or_si64(_mm_and_si64(m, a),
		       _mm_andnot_si64(m,b));
}

static __m64 MM64_max_si64(__m64 a, __m64 b)
{
    __m64 m = (__m64) MM64_cmpgt_si64(a, b);
    return _mm_or_si64(_mm_and_si64(m, a),
		       _mm_andnot_si64(m,b));
}


static __m64 MM64_min_pi8(__m64 a, __m64 b)
{
    __m64 m = (__m64) _mm_cmpgt_pi8(b, a);
    return _mm_or_si64(_mm_and_si64(m, a),
		       _mm_andnot_si64(m,b));
}


static __m64 MM64_min_pi16(__m64 a, __m64 b)
{
    __m64 m = (__m64) _mm_cmpgt_pi16(b, a);
    return _mm_or_si64(_mm_and_si64(m, a),
		       _mm_andnot_si64(m,b));
}

static __m64 MM64_min_pi32(__m64 a, __m64 b)
{
    __m64 m = (__m64) _mm_cmpgt_pi32(b, a);
    return _mm_or_si64(_mm_and_si64(m, a),
		       _mm_andnot_si64(m,b));
}

static __m64 MM64_min_si64(__m64 a, __m64 b)
{
    __m64 m = (__m64) MM64_cmpgt_si64(b, a);
    return _mm_or_si64(_mm_and_si64(m, a),
		       _mm_andnot_si64(m,b));
}


#endif  // __MMX__
#endif  // __VEC_AVX512_H__
