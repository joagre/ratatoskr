// NEON vectors
// /usr/lib/gcc/arm-linux-gnueabihf/10/include/arm_neon.h
//  echo | gcc -dM -E -//
#ifndef __VEC_NEON_H__

#include <stdint.h>
#include <memory.h>

#if defined(__ARM_NEON__)
// #warning including NEON
#include <arm_neon.h>

#include "vec_undef.h"

#define vec_native_t int32x4_t
#define vec_int_t    int32x4_t
#define vec_flt_t    float32x4_t
#define vec_dbl_t    float32x4_t  // no 64 bit float...
// vector access types
#define vec_int8_t    int8x16_t
#define vec_int16_t   int16x8_t
#define vec_int32_t   int32x4_t
#define vec_int64_t   int32x4_t  // int64x2_t
#define vec_float32_t  vec_flt_t
#define vec_float64_t  vec_dbl_t
// element access types
#define elem_int8_t    int8x16_t
#define elem_int16_t   int16x8_t
#define elem_int32_t   int32x4_t
#define elem_int64_t   int32x4_t   // int64x2_t
#define elem_float32_t float32x4_t
#define elem_float64_t float32x4_t // float64x2_t

#define vecop_neg_int8      vnegq_s8
#define vecop_neg_int16     vnegq_s16
#define vecop_neg_int32     vnegq_s32
#define vecop_neg_int64     vnegq_s32 // vnegq_s64
#define vecop_neg_float32   vnegq_f32
#define vecop_neg_float64   vnegq_f32 // vnegq_f64

#define vecop_add_int8      vaddq_s8
#define vecop_add_int16     vaddq_s16
#define vecop_add_int32     vaddq_s32
#define vecop_add_int64     vaddq_s32 // vaddq_s64
#define vecop_add_float32   vaddq_f32
#define vecop_add_float64   vaddq_f32 // vaddq_f64

#define vecop_sub_int8      vsubq_s8
#define vecop_sub_int16     vsubq_s16
#define vecop_sub_int32     vsubq_s32
#define vecop_sub_int64     vsubq_s32 // vsubq_s64
#define vecop_sub_float32   vsubq_f32
#define vecop_sub_float64   vsubq_f32 // vsubq_f64

#define vecop_min_int8      vminq_s8
#define vecop_min_int16     vminq_s16
#define vecop_min_int32     vminq_s32
#define vecop_min_int64     vminq_s32 // vminq_s64
#define vecop_min_float32   vminq_f32
#define vecop_min_float64   vminq_f32 // vminq_f64

#define vecop_max_int8      vmaxq_s8
#define vecop_max_int16     vmaxq_s16
#define vecop_max_int32     vmaxq_s32
#define vecop_max_int64     vmaxq_s32 // vmaxq_s64
#define vecop_max_float32   vmaxq_f32
#define vecop_max_float64   vmaxq_f32 // vmaxq_f64

#define vecop_band_bits     vandq_s8
#define vecop_bor_bits      vorrq_s8
#define vecop_bxor_bits     veorq_s8
#define vecop_bnot_bits     VBNOT_s8

#define vecop_cmpgt_int8    vcgtq_s8
#define vecop_cmpgt_int16   vcgtq_s16
#define vecop_cmpgt_int32   vcgtq_s32
#define vecop_cmpgt_int64   vcgtq_s32 // vcgtq_s64
#define vecop_cmpgt_float32  vcgtq_f32
#define vecop_cmpgt_float64  vcgtq_f32 // vcgtq_f64

#define vecop_cmplt_int8    vcltq_s8
#define vecop_cmplt_int16   vcltq_s16
#define vecop_cmplt_int32   vcltq_s32
#define vecop_cmplt_int64   vcltq_s32  // vcltq_s64
#define vecop_cmplt_float32 vcltq_f32
#define vecop_cmplt_float64 vcltq_f32  // vcltq_f64

static int8x16_t VBNOT_s8(int8x16_t x)
{
    return ~x;
}


#endif // __ARM_NEON__
#endif // __VEC_NEON_H__
