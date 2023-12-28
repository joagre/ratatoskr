#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include "log.h"

void log_entry(log_level_t log_level, char* file, uint32_t line, char* format,
               ...) {
    va_list args;
    va_start(args, format);
    switch (log_level) {
	case LOG_LEVEL_DEBUG:
	    fprintf(stderr, "DEBUG (%s: %d): ", file, line);
	    break;
	case LOG_LEVEL_INFO:
	    fprintf(stderr, "INFO (%s: %d): ", file, line);
	    break;
	case LOG_LEVEL_WARNING:
	    fprintf(stderr, "WARNING (%s: %d): ", file, line);
	    break;
	case LOG_LEVEL_ERROR:
	    fprintf(stderr, "ERROR (%s: %d): ", file, line);
	    break;
	case LOG_LEVEL_PANIC:
	    fprintf(stderr, "PANIC (%s: %d): ", file, line);
	    break;
    }
    vfprintf(stderr, format, args);
    fprintf(stderr, "\n");
    va_end(args);
}

void log_satie_error(log_level_t log_level, char* file, uint32_t line,
		     satie_error_t* error) {
    char buf[1024];
    log_entry(log_level, file, line, "%s",
	      satie_error_to_string(error, buf, sizeof(buf)));
}

void log_abort(char* file, uint32_t line, char* message, ...) {
    va_list args;
    va_start(args, message);
    log_entry(LOG_LEVEL_PANIC, file, line, message, args);
    va_end(args);
    abort();
}

void log_assert(char* file, uint32_t line, bool condition, char* message, ...) {
    if (!condition) {
        va_list args;
        va_start(args, message);
        log_entry(LOG_LEVEL_PANIC, file, line, message, args);
        va_end(args);
        abort();
    }
}
