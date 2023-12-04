#include <stdlib.h>
#include <errno.h>
#include "util.h"

long string_to_long(char* string, satie_error_t* error) {
    errno = 0;
    char *endptr;
    long value = strtol(string, &endptr, 10);
    if (string == endptr || *endptr != '\0' || errno != 0) {
        SET_ERROR_MESSAGE(error, COMPONENT_GENERAL, "Not a number: %s", string);
    } else {
        CLEAR_ERROR(error);
    }
    return value;
}
