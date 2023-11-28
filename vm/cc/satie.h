#ifndef __SATIE_H__
#define __SATIE_H__

#include <stdbool.h>

#define SUCCESS 0
#define PARAMETER_ERROR 1

#define DEFAULT_CHECK_AFTER 100
#define DEFAULT_LOAD_PATH "./"
#define DEFAULT_TIME_SLICE 25

typedef struct {
    bool success;
    long value;
} satie_result_t;

void usage(const char* name);
satie_result_t string_to_long(const char* string);

#endif
