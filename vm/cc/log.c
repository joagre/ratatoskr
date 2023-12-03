#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include "log.h"

void satie_log(log_level_t log_level, char*file_path, char* format, ...) {
    va_list args;
    va_start(args, format);
    switch (log_level) {
    case LOG_LEVEL_DEBUG:
        fprintf(stderr, "DEBUG (%s): ", file_path);
        break;
    case LOG_LEVEL_INFO:
        fprintf(stderr, "INFO (%s): ", file_path);
        break;
    case LOG_LEVEL_WARNING:
        fprintf(stderr, "WARNING (%s): ", file_path);
        break;
    case LOG_LEVEL_ERROR:
        fprintf(stderr, "ERROR (%s): ", file_path);
        break;
    case LOG_LEVEL_PANIC:
        fprintf(stderr, "PANIC (%s): ", file_path);
        break;
    }
    vfprintf(stderr, format, args);
    fprintf(stderr, "\n");
    va_end(args);
}

void satie_abort(char*file_path, char* message) {
    satie_log(LOG_LEVEL_PANIC, file_path, message);
    abort();
}

void satie_assert(char *file_path, bool condition, char* message) {
    if (!condition) {
        satie_log(LOG_LEVEL_PANIC, file_path, message);
        abort();
    }
}
