#include <stdlib.h>
#include <stdio.h>
#include "log.h"

void vm_log(log_level_t log_level, const char* message) {
    switch (log_level) {
    case LOG_LEVEL_DEBUG:
        fprintf(stderr, "DEBUG: %s\n", message);
        break;
    case LOG_LEVEL_INFO:
        fprintf(stderr, "INFO: %s\n", message);
        break;
    case LOG_LEVEL_WARNING:
        fprintf(stderr, "WARNING: %s\n", message);
        break;
    case LOG_LEVEL_ERROR:
        fprintf(stderr, "ERROR: %s\n", message);
        break;
    case LOG_LEVEL_PANIC:
        fprintf(stderr, "PANIC: %s\n", message);
        break;
    }
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
