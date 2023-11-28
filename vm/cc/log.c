#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include "log.h"

void vm_log(log_level_t log_level, const char* format, ...) {
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

void vm_abort(const char* message) {
    vm_log(LOG_LEVEL_PANIC, message);
    abort();
}

void vm_assert(bool condition, const char* message) {
    if (!condition) {
        vm_log(LOG_LEVEL_PANIC, message);
        abort();
    }
}
