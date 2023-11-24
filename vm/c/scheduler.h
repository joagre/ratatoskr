#ifndef __SCHEDULER_H__
#define __SCHEDULER_H__

#include <stdint.h>
#include "job.h"
#include "dlist.h"

typedef struct {
    dlist_t ready_queue;
    dlist_t waiting_queue;
    uint32_t time_slice;
    uint32_t check_after;
    job_t* running_job;
} scheduler_t;

typedef struct {
    char* message;
} scheduler_error_t;

void scheduler_init(scheduler_t* scheduler);
uint32_t scheduler_next_jid(void);
void scheduler_run(scheduler_t* scheduler);
void scheduler_spawn(scheduler_t* scheduler, job_t* job);
void scheduler_send_message(scheduler_t* scheduler, uint32_t jid, long value);
job_t* scheduler_find_job(scheduler_t* scheduler, uint32_t jid);
void remove_from_waiting_queue(scheduler_t *scheduler, uint32_t jid);

#endif
