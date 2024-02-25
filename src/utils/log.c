#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include "log.h"

void log_entry_variadic(log_level_t log_level, char* file, uint32_t line,
			char* format, ...) {
    va_list args;
    va_start(args, format);
    log_entry(log_level, file, line, format, args);
    va_end(args);
}

void log_entry(log_level_t log_level, char* file, uint32_t line, char* format,
               va_list args) {
    switch (log_level) {
	case LOG_LEVEL_DEBUG:
	    printf("DEBUG (%s: %d): ", file, line);
	    break;
	case LOG_LEVEL_INFO:
	    printf("INFO (%s: %d): ", file, line);
	    break;
	case LOG_LEVEL_WARNING:
	    printf("WARNING (%s: %d): ", file, line);
	    break;
	case LOG_LEVEL_ERROR:
	    printf("ERROR (%s: %d): ", file, line);
	    break;
	case LOG_LEVEL_PANIC:
	    printf("PANIC (%s: %d): ", file, line);
	    break;
    }
    vfprintf(stderr, format, args);
    printf("\n");
}

void log_satie_error(log_level_t log_level, char* file, uint32_t line,
		     satie_error_t* error) {
    char buf[1024];
    satie_error_to_string(error, buf, sizeof(buf));
    log_entry_variadic(log_level, file, line, "%s", buf);
}

void log_abort(char* file, uint32_t line, char* message, ...) {
    va_list args;
    va_start(args, message);
    log_entry(LOG_LEVEL_PANIC, file, line, message, args);
    va_end(args);
    fflush(stdout);
    abort();
}

void log_assert(char* file, uint32_t line, bool condition, char* message, ...) {
    if (!condition) {
        va_list args;
        va_start(args, message);
        log_entry(LOG_LEVEL_PANIC, file, line, message, args);
        va_end(args);
	fflush(stdout);
        abort();
    }
}
