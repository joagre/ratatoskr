#ifndef __LOG_H__
#define __LOG_H__

#include <stdbool.h>

typedef enum {
    LOG_LEVEL_DEBUG,
    LOG_LEVEL_INFO,
    LOG_LEVEL_WARNING,
    LOG_LEVEL_ERROR,
    LOG_LEVEL_PANIC
} log_level_t;

void satie_log(log_level_t log_level, const char* format, ...);
void satie_abort(const char* message);
void satie_assert(bool condition, const char* message);

#ifdef DEBUG
#define SATIE_LOG(log_level, message, ...) ({    \
   satie_log(log_level, message, ##__VA_ARGS__); \
})
#define SATIE_ABORT( message) ({ \
    satie_abort(message);        \
})
#define SATIE_ASSERT(condition, message) ({ \
    satie_assert(condition, message);       \
})
#else
#define SATIE_LOG(log_level, message, ...) ((void)0)
#define SATIE_ABORT(message) ((void)0)
#define SATIE_ASSERT(condition, message) ((void)0)
#endif

#endif
