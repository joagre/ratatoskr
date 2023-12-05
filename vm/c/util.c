#include <stdlib.h>
#include <errno.h>
#include <time.h>
#include "util.h"

void sleep_ms(uint32_t ms) {
    struct timespec ts;
    ts.tv_sec = ms / 1000;
    ts.tv_nsec = (ms % 1000) * 1000000;
    nanosleep(&ts, NULL);
}

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
