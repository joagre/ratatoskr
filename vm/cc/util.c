#include <stdlib.h>
#include <errno.h>
#include "util.h"

long string_to_long(const char* string, satie_error_t* satie_error) {
    errno = 0;
    char *endptr;
    long value = strtol(string, &endptr, 10);
    if (string == endptr || *endptr != '\0' || errno != 0) {
        SET_ERROR(satie_error, ERROR_TYPE_MESSAGE, COMPONENT_GENERAL);
        satie_error->message = "Not a number";
    } else {
        CLEAR_ERROR(satie_error);
    }
    return value;
}
