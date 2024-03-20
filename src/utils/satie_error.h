#ifndef UTILS_SATIE_H
#define UTILS_SATIE_H

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

#define GENERATE_ENUM(ENUM) ENUM,
#define GENERATE_STRING(STRING) #STRING,

#define FOREACH_ERROR_TYPE(TYPE) \
	TYPE(ERROR_TYPE_NONE) \
	TYPE(ERROR_TYPE_CODE) \
	TYPE(ERROR_TYPE_ERRNO) \
	TYPE(ERROR_TYPE_MESSAGE)

typedef enum {
    FOREACH_ERROR_TYPE(GENERATE_ENUM)
} satie_error_type_t;

#define FOREACH_COMPONENT(COMPONENT) \
	COMPONENT(COMPONENT_GENERAL) \
	COMPONENT(COMPONENT_VM) \
	COMPONENT(COMPONENT_LOADER) \
	COMPONENT(COMPONENT_INTERPRETER) \
	COMPONENT(COMPONENT_COMPILER) \
	COMPONENT(COMPONENT_SCHEDULER)

typedef enum {
    FOREACH_COMPONENT(GENERATE_ENUM)
} satie_component_t;

#define SET_ERROR_NONE(error, component) ({ \
    if (error != NULL) { \
        (error)->failed = true; \
        (error)->flags = (((uint16_t)(component) << 8) | ERROR_TYPE_NONE); \
    } \
})
#define SET_ERROR_CODE(error, component, code) ({    \
    if (error != NULL) { \
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
    if (error != NULL) { \
        (error)->failed = true; \
        (error)->flags = (((uint16_t)(component) << 8) | ERROR_TYPE_MESSAGE); \
        snprintf(satie_error_message, MAX_ERROR_MESSAGE_SIZE, format, ##__VA_ARGS__); \
    } \
})
#define CLEAR_ERROR(error) ({ \
    if (error != NULL) { \
        (error)->failed = false; \
        (error)->flags = 0; \
    } \
})
#define IS_COMPONENT(error, component) (((error)->flags >> 8) == (component))
#define IS_ERROR_TYPE(error, type) (((error)->flags & 0xFF) == (type))
#define GET_COMPONENT(error) ((error)->flags >> 8)
#define GET_ERROR_TYPE(error) ((error)->flags & 0xFF)

char* satie_error_to_string(satie_error_t *error, char* buf, size_t bufsiz);

#endif
