#ifndef __INTERPRETER_H__
#define __INTERPRETER_H__

#include <time.h>
#include <stdint.h>
#include "scheduler.h"
#include "loader.h"

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

typedef struct interpreter {
    loader_t* loader;
} interpreter_t;

struct scheduler; // Forward declaration of circular dependency

void interpreter_init(interpreter_t *interpreter, loader_t* loader);
void interpreter_clear(interpreter_t *interpreter);
interpreter_result_t interpreter_run(interpreter_t* interpreter,
				     struct scheduler* scheduler);
uint32_t interpreter_mspawn(interpreter_t* interpreter,
			    struct scheduler* scheduler, char* module_name,
			    vm_label_t label, vm_stack_value_t* parameters,
			    uint8_t arity, satie_error_t* error);

#endif
