#ifndef __UTIL_H__
#define __UTIL_H__

#include "satie_error.h"

void sleep_ms(uint32_t ms);
long string_to_long(char* string, satie_error_t* error);

#endif
