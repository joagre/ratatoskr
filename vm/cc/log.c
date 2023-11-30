#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include "log.h"

void satie_log(log_level_t log_level, char* format, ...) {
    va_list args;
    va_start(args, format);
    switch (log_level) {
    case LOG_LEVEL_DEBUG:
        fprintf(stderr, "DEBUG: ");
        break;
    case LOG_LEVEL_INFO:
        fprintf(stderr, "INFO: ");
        break;
    case LOG_LEVEL_WARNING:
        fprintf(stderr, "WARNING: ");
        break;
    case LOG_LEVEL_ERROR:
        fprintf(stderr, "ERROR: ");
        break;
    case LOG_LEVEL_PANIC:
        fprintf(stderr, "PANIC: ");
        break;
    }
    vfprintf(stderr, format, args);
    fprintf(stderr, "\n");
    va_end(args);
}

void satie_abort(char* message) {
    satie_log(LOG_LEVEL_PANIC, message);
    abort();
}

void satie_assert(bool condition, char* message) {
    if (!condition) {
        satie_log(LOG_LEVEL_PANIC, message);
        abort();
    }
}
