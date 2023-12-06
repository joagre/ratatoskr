#ifndef __SCHEDULER_H__
#define __SCHEDULER_H__

#include <stdint.h>
#include "ready_queue.h"
#include "waiting_queue.h"
#include "interpreter.h"
#include "loader.h"
#include "job.h"

struct interpreter;  // Forward declaration of circular dependency

typedef struct scheduler {
    ready_queue_t ready_queue;
    waiting_queue_t waiting_queue;
    loader_t* loader;
    struct interpreter* interpreter;
    uint32_t time_slice;
    uint16_t check_after;
    job_t* running_job;
} scheduler_t;


void scheduler_init(scheduler_t* scheduler, loader_t* loader,
                    struct interpreter* interpreter, uint32_t time_slice,
                    uint16_t check_after);
void scheduler_free(scheduler_t* scheduler);
uint32_t scheduler_next_jid(void);
void scheduler_run(scheduler_t *scheduler);
void scheduler_spawn(scheduler_t* scheduler, job_t* job);
void scheduler_send_message(scheduler_t* scheduler, uint32_t jid,
                            vm_stack_value_t value);

#endif
