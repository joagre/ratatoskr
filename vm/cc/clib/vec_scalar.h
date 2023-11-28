// non simd vectors
#ifndef __VEC_SCALAR_H__
#define __VEC_SCALAR_H__
#include <stdint.h>
#include <memory.h>

#include "vec_undef.h"

typedef int64_t* vec_int_t;
typedef float32_t* vec_flt_t;
typedef float64_t* vec_dbl_t;

#define vec_scalar   1
#define vec_native_t int8_t
#define vec_int_t    int8_t
#define vec_flt_t    vec_flt_t
#define vec_dbl_t    vec_dbl_t
// vector access types
#define vec_int8_t     int8_t
#define vec_int16_t    int16_t
#define vec_int32_t    int32_t
#define vec_int64_t    int64_t
#define vec_float32_t    float32_t
#define vec_float64_t    float64_t
// element access types
#define elem_int8_t      int8_t
#define elem_int16_t     int16_t
#define elem_int32_t     int32_t
#define elem_int64_t     int64_t
#define elem_float32_t   float32_t
#define elem_float64_t   float64_t

#define vecop_neg_int8      _scalar_neg_int8
#define vecop_neg_int16     _scalar_neg_int16
#define vecop_neg_int32     _scalar_neg_int32
#define vecop_neg_int64     _scalar_neg_int64
#define vecop_neg_float32     _scalar_neg_float32
#define vecop_neg_float64     _scalar_neg_float64

#define vecop_add_int8      _scalar_add_int8
#define vecop_add_int16     _scalar_add_int16
#define vecop_add_int32     _scalar_add_int32
#define vecop_add_int64     _scalar_add_int64
#define vecop_add_float32     _scalar_add_float32
#define vecop_add_float64     _scalar_add_float64

#define vecop_sub_int8      _scalar_sub_int8
#define vecop_sub_int16     _scalar_sub_int16
#define vecop_sub_int32     _scalar_sub_int32
#define vecop_sub_int64     _scalar_sub_int64
#define vecop_sub_float32     _scalar_sub_float32
#define vecop_sub_float64     _scalar_sub_float64

#define vecop_min_int8      _scalar_min_int8
#define vecop_min_int16     _scalar_min_int16
#define vecop_min_int32     _scalar_min_int32
#define vecop_min_int64     _scalar_min_int64
#define vecop_min_float32     _scalar_min_float32
#define vecop_min_float64     _scalar_min_float64

#define vecop_max_int8      _scalar_max_int8
#define vecop_max_int16     _scalar_max_int16
#define vecop_max_int32     _scalar_max_int32
#define vecop_max_int64     _scalar_max_int64
#define vecop_max_float32     _scalar_max_float32
#define vecop_max_float64     _scalar_max_float64

#define vecop_cmpgt_int8    _scalar_cmpgt_int8
#define vecop_cmpgt_int16   _scalar_cmpgt_int16
#define vecop_cmpgt_int32   _scalar_cmpgt_int32
#define vecop_cmpgt_int64   _scalar_cmpgt_int64
#define vecop_cmpgt_float32   _scalar_cmpgt_float32
#define vecop_cmpgt_float64   _scalar_cmpgt_float64

#define vecop_cmplt_int8    _scalar_cmplt_int8
#define vecop_cmplt_int16   _scalar_cmplt_int16
#define vecop_cmplt_int32   _scalar_cmplt_int32
#define vecop_cmplt_int64   _scalar_cmplt_int64
#define vecop_cmplt_float32   _scalar_cmplt_float32
#define vecop_cmplt_float64   _scalar_cmplt_float64

#define vecop_band_bits        _scalar_and_int64
#define vecop_bor_bits         _scalar_or_int64
#define vecop_bxor_bits        _scalar_xor_int64
#define vecop_bnot_bits        _scalar_bnot_int64

static inline int8_t _scalar_neg_int8(int8_t a) { return -a; }
static inline int16_t _scalar_neg_int16(int16_t a) { return -a; }
static inline int32_t _scalar_neg_int32(int32_t a) { return -a; }
static inline int64_t _scalar_neg_int64(int64_t a) { return -a; }
static inline float32_t _scalar_neg_float32(float32_t a) { return -a; }
static inline float64_t _scalar_neg_float64(float64_t a) { return -a; }

static inline int8_t _scalar_add_int8(int8_t a,int8_t b) { return a+b; }
static inline int16_t _scalar_add_int16(int16_t a,int16_t b) { return a+b; }
static inline int32_t _scalar_add_int32(int32_t a,int32_t b) { return a+b; }
static inline int64_t _scalar_add_int64(int64_t a,int64_t b) { return a+b; }
static inline float32_t _scalar_add_float32(float32_t a,float32_t b) { return a+b; }
static inline float64_t _scalar_add_float64(float64_t a,float64_t b) { return a+b; }

static inline int8_t _scalar_sub_int8(int8_t a,int8_t b) { return a-b; }
static inline int16_t _scalar_sub_int16(int16_t a,int16_t b) { return a-b; }
static inline int32_t _scalar_sub_int32(int32_t a,int32_t b) { return a-b; }
static inline int64_t _scalar_sub_int64(int64_t a,int64_t b) { return a-b; }
static inline float32_t _scalar_sub_float32(float32_t a,float32_t b) { return a-b; }
static inline float64_t _scalar_sub_float64(float64_t a,float64_t b) { return a-b; }

#define SMIN(a, b) (((a)<(b))?(a):(b))
static inline int8_t _scalar_min_int8(int8_t a,int8_t b) { return SMIN(a,b); }
static inline int16_t _scalar_min_int16(int16_t a,int16_t b) { return SMIN(a,b); }
static inline int32_t _scalar_min_int32(int32_t a,int32_t b) { return SMIN(a,b); }
static inline int64_t _scalar_min_int64(int64_t a,int64_t b) { return SMIN(a,b); }
static inline float32_t _scalar_min_float32(float32_t a,float32_t b) { return SMIN(a,b); }
static inline float64_t _scalar_min_float64(float64_t a,float64_t b) { return SMIN(a,b); }

#define SMAX(a, b) (((a)>(b))?(a):(b))
static inline int8_t _scalar_max_int8(int8_t a,int8_t b) { return SMAX(a,b); }
static inline int16_t _scalar_max_int16(int16_t a,int16_t b) { return SMAX(a,b); }
static inline int32_t _scalar_max_int32(int32_t a,int32_t b) { return SMAX(a,b); }
static inline int64_t _scalar_max_int64(int64_t a,int64_t b) { return SMAX(a,b); }
static inline float32_t _scalar_max_float32(float32_t a,float32_t b) { return SMAX(a,b); }
static inline float64_t _scalar_max_float64(float64_t a,float64_t b) { return SMAX(a,b); }

#define CMPGT(a, b) (((a)>(b))?(-1):(0))
static inline int8_t _scalar_cmpgt_int8(int8_t a,int8_t b) { return CMPGT(a,b); }
static inline int16_t _scalar_cmpgt_int16(int16_t a,int16_t b) { return CMPGT(a,b); }
static inline int32_t _scalar_cmpgt_int32(int32_t a,int32_t b) { return CMPGT(a,b); }
static inline int64_t _scalar_cmpgt_int64(int64_t a,int64_t b) { return CMPGT(a,b); }
static inline float32_t _scalar_cmpgt_float32(float32_t a,float32_t b) { return CMPGT(a,b); }
static inline float64_t _scalar_cmpgt_float64(float64_t a,float64_t b) { return CMPGT(a,b); }

#define CMPLT(a, b) (((a)<(b))?(-1):(0))
static inline int8_t _scalar_cmplt_int8(int8_t a,int8_t b) { return CMPLT(a,b); }
static inline int16_t _scalar_cmplt_int16(int16_t a,int16_t b) { return CMPLT(a,b); }
static inline int32_t _scalar_cmplt_int32(int32_t a,int32_t b) { return CMPLT(a,b); }
static inline int64_t _scalar_cmplt_int64(int64_t a,int64_t b) { return CMPLT(a,b); }
static inline float32_t _scalar_cmplt_float32(float32_t a,float32_t b) { return CMPLT(a,b); }
static inline float64_t _scalar_cmplt_float64(float64_t a,float64_t b) { return CMPLT(a,b); }

static inline int64_t _scalar_and_int64(int64_t a,int64_t b) { return a&b; }
static inline int64_t _scalar_or_int64(int64_t a,int64_t b) { return a|b; }
static inline int64_t _scalar_xor_int64(int64_t a,int64_t b) { return a^b; }
static inline int64_t _scalar_bnot_int64(int64_t a) { return ~a; }

#endif  // __VEC_SCALAR_H__

