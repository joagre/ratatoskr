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

void satie_log(log_level_t log_level, char*file_path, char* format, ...);
void satie_abort(char*file_path, char* message);
void satie_assert(char *file_path, bool condition, char* message);

#ifdef DEBUG
// LOG_DEBUG
#ifndef MUTE_LOG_DEBUG
#define LOG_DEBUG(message, ...) ({           \
    satie_log(LOG_LEVEL_DEBUG, __FILE__ , message, ##__VA_ARGS__); \
})
#else
#define LOG_DEBUG(message, ...) ((void)0)
#endif

// LOG_INFO
#ifndef MUTE_LOG_INFO
#define LOG_INFO(message, ...) ({           \
    satie_log(LOG_LEVEL_INFO, __FILE__ , message, ##__VA_ARGS__); \
})
#else
#define LOG_INFO(message, ...) ((void)0)
#endif

// LOG_WARNING
#ifndef MUTE_LOG_WARNING
#define LOG_WARNING(message, ...) ({           \
    satie_log(LOG_LEVEL_WARNING, __FILE__ , message, ##__VA_ARGS__); \
})
#else
#define LOG_WARNING(message, ...) ((void)0)
#endif

// LOG_ERROR
#define LOG_ERROR(message, ...) ({     \
    satie_log(LOG_LEVEL_ERROR, __FILE__, message, ##__VA_ARGS__); \
})

// LOG_PANIC
#define LOG_PANIC(message, ...) ({     \
    satie_log(LOG_LEVEL_PANIC, __FILE__, message, ##__VA_ARGS__); \
})

// LOG_ABORT
#define LOG_ABORT(message) ({ \
    satie_abort(__FILE__, message); \
})

// LOG_ASSERT
#define LOG_ASSERT(condition, message) ({ \
    satie_assert(__FILE__, condition, message); \
})
#else
#define LOG_DEBUG(message, ...) ((void)0)
#define LOG_INFO(message, ...) ((void)0)
#define LOG_WARNING(message, ...) ((void)0)
#define LOG_ERROR(message, ...) ((void)0)
#define LOG_PANIC(message, ...) ((void)0)
#define LOG_ABORT(message) ((void)0)
#define LOG_ASSERT(condition, message) ((void)0)
#endif

#endif
