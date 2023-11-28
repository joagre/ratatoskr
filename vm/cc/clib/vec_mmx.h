// MMX vectors
#ifndef __VEC_MMX_H__

#include <stdint.h>
#include <memory.h>

#if defined(__MMX__)
#warning including MMX
#include <mmintrin.h>

#include "vec_undef.h"

#define vec_native_t __m64
#define vec_int_t    __m64i

#endif  // __MMX__
#endif  // __VEC_AVX512_H__
