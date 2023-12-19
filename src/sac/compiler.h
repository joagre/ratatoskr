#ifndef __COMPILER_H__
#define __COMPILER_H__

#include <satie_error.h>
#include <stdint.h>
#include <vm.h>

#define APPEND_VALUE(loader, T, value) ({       \
    uint8_t bytes[sizeof(T)]; \
    memcpy(bytes, &(value), sizeof(T)); \
    append_bytes(loader, sizeof(T), bytes); \
})

typedef struct {
    uint8_t* byte_code;
    uint32_t byte_code_size;
    uint32_t max_byte_code_size;
} compiler_t;

void compile(char* input_filename, char *output_directory, satie_error_t* error);

#endif
