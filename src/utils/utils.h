#ifndef UTILS_UTILS_H
#define UTILS_UTILS_H

#include "satie_error.h"
#include <stdint.h>

#define UTILS_INITIAL_BUFSIZ 512

void buf_append(uint8_t** buf, uint32_t* max_bufsiz, uint32_t* bufsiz,
                uint8_t* bytes, uint16_t n);
bool is_valid_extension(char *filename, char *extension);
void sleep_ms(uint32_t ms);
long string_to_long(char* string, satie_error_t* error);
char* strip_extension(char *filename);

#endif
