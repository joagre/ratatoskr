#ifndef __SCHEDULER_H__
#define __SCHEDULER_H__

#include <stdint.h>
#include <satie_error.h>
#include "ready_queue.h"
#include "waiting_queue.h"
#include "interpreter.h"
#include "job.h"

struct interpreter;  // Forward declaration of circular dependency

typedef struct scheduler {
    ready_queue_t ready_queue;
    waiting_queue_t waiting_queue;
    struct interpreter* interpreter;
    uint32_t time_slice;
    uint16_t check_after;
    job_t* running_job;
} scheduler_t;


void scheduler_init(scheduler_t* scheduler, struct interpreter* interpreter,
		    uint32_t time_slice, uint16_t check_after);
void scheduler_clear(scheduler_t* scheduler);
uint32_t scheduler_next_jid(void);
void scheduler_run(scheduler_t *scheduler);
void scheduler_spawn(scheduler_t* scheduler, job_t* job);
void scheduler_send_message(scheduler_t* scheduler, uint32_t jid,
                            vm_stack_value_t value, satie_error_t* error);

#endif
