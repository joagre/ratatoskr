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

void vm_log(log_level_t log_level, const char* message);
void vm_abort(const char* message);
void vm_assert(bool condition, const char* message);

#ifdef DEBUG
#define VM_LOG(log_level, message) ({ \
    vm_log(log_level, message);       \
})
#define VM_ABORT( message) ({ \
    vm_abort(message);        \
})
#define VM_ASSERT(condition, message) ({ \
    vm_assert(condition, message);       \
})
#else
#define VM_LOG(log_level, message) ((void)0)
#define VM_ABORT(message) ((void)0)
#define VM_ASSERT(condition, message) ((void)0)
#endif

#endif
