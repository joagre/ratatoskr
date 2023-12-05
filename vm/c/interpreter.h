#ifndef __INTERPRETER_H__
#define __INTERPRETER_H__

#include <time.h>
#include <stdint.h>
#include "scheduler.h"

#define MILLIS_PER_SEC 1000
#define MS_TO_CLOCK(ms) ((ms) * CLOCKS_PER_SEC / MILLIS_PER_SEC)
#define START_TIMER() clock()
#define ELAPSED_TIME_MS(start_time) ((uint64_t)(((clock() - (start_time)) * MILLIS_PER_SEC) / CLOCKS_PER_SEC))

typedef enum {
    INTERPRETER_RESULT_HALT = 0,
    INTERPRETER_RESULT_TIMEOUT,
    INTERPRETER_RESULT_RECV,
    INTERPRETER_RESULT_EXIT
} interpreter_result_t;

typedef enum {
    INTERPRETER_MODE_STACK = 0,
    INTERPRETER_MODE_REGISTER
} interpreter_mode_t;

typedef struct interpreter {
    interpreter_mode_t mode;
} interpreter_t;

struct scheduler;  // Forward declaration

void interpreter_init(interpreter_t *interpreter, interpreter_mode_t mode);
interpreter_result_t interpreter_run(struct scheduler* scheduler);

#endif
