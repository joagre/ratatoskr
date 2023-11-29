#ifndef __SATIE_H__
#define __SATIE_H__

#include <stdbool.h>
#include <stdint.h>

typedef struct {
    bool failed;
    uint16_t flags;
    union {
        int code;
        const char *message;
    };
} satie_error_t;

typedef enum {
    ERROR_TYPE_NONE = 0,
    ERROR_TYPE_CODE,
    ERROR_TYPE_MESSAGE,
} satie_error_type_t;

typedef enum {
    COMPONENT_GENERAL = 0,
    COMPONENT_VM,
    COMPONENT_LOADER,
    COMPONENT_INTERPRETER
} satie_component_t;

#define SET_ERROR(error, type, component) ({ \
    if (error) { \
        (error)->failed = true; \
        (error)->flags = (((uint16_t)(component) << 8) | ((uint16_t)(type) & 0xFF)); \
    } \
})
#define SET_ERROR_TYPE(error, type) ({ \
    if (error) { \
        (error)->flags = (((error)->flags & 0xFF00) | ((uint16_t)(type) & 0xFF)); \
    } \
})
#define SET_COMPONENT(error, component) ({ \
    if (error) { \
        (error)->flags = (((error)->flags & 0x00FF) | (((uint16_t)(component) << 8) & 0xFF00)); \
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

void satie_print_error(const satie_error_t *error);

#endif
