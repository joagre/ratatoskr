// AVX512 vectors
#ifndef __VEC_AVX512_H__

#include <stdint.h>
#include <memory.h>

#if defined(__AVX512F__)
#warning including AVX512
#include <immintrin.h>

#include "vec_undef.h"

#define vec_native_t __m512
#define vec_int_t    __m512i


#endif  // __AVX512F__
#endif  // __VEC_AVX512_H__

