#ifndef __SATIE_H__
#define __SATIE_H__

#include <stdbool.h>
#include <stdio.h>
#include <string.h>
#include <stdint.h>

#define MAX_ERROR_MESSAGE_SIZE 1024

// Used to store error messages
extern char satie_error_message[MAX_ERROR_MESSAGE_SIZE];

typedef struct {
    bool failed;
    uint16_t flags;
    union {
        int code;
        int errno_value;
    };
} satie_error_t;

typedef enum {
    ERROR_TYPE_NONE = 0,
    ERROR_TYPE_CODE,
    ERROR_TYPE_ERRNO,
    ERROR_TYPE_MESSAGE,
} satie_error_type_t;

typedef enum {
    COMPONENT_GENERAL = 0,
    COMPONENT_VM,
    COMPONENT_LOADER,
    COMPONENT_INTERPRETER
} satie_component_t;

#define SET_ERROR_NONE(error, component) ({ \
    if (error) { \
        (error)->failed = true; \
        (error)->flags = (((uint16_t)(component) << 8) | ERROR_TYPE_NONE); \
    } \
})
#define SET_ERROR_CODE(error, component, code) ({    \
    if (error) { \
        (error)->failed = true; \
        (error)->flags = (((uint16_t)(component) << 8) | ERROR_TYPE_CODE); \
        (error)->code = (code); \
    } \
})
#define SET_ERROR_ERRNO(error, component) ({   \
    if (error) { \
        (error)->failed = true; \
        (error)->flags = (((uint16_t)(component) << 8) | ERROR_TYPE_ERRNO); \
        (error)->errno_value = errno; \
    } \
})
#define SET_ERROR_MESSAGE(error, component, format, ...) ({ \
    if (error) { \
        (error)->failed = true; \
        (error)->flags = (((uint16_t)(component) << 8) | ERROR_TYPE_MESSAGE); \
        snprintf(satie_error_message, MAX_ERROR_MESSAGE_SIZE, format, ##__VA_ARGS__); \
    } \
})
#define CLEAR_ERROR(error) ({ \
    if (error) { \
        (error)->failed = false; \
        (error)->flags = 0; \
    } \
})
#define IS_COMPONENT(error, component) (((error)->flags >> 8) == (component))
#define IS_ERROR_TYPE(error, type) (((error)->flags & 0xFF) == (type))
#define GET_COMPONENT(error) ((error)->flags >> 8)
#define GET_ERROR_TYPE(error) ((error)->flags & 0xFF)

void satie_print_error(satie_error_t *error);

#endif
