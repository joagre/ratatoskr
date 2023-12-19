#ifndef __UTILS_H__
#define __UTILS_H__

#include "satie_error.h"

bool is_valid_extension(char *filename, char *extension);
void sleep_ms(uint32_t ms);
long string_to_long(char* string, satie_error_t* error);
char* strip_extension(char *filename);

#endif
