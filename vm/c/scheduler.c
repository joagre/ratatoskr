#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <assert.h>
#include "job.h"
#include "interpreter.h"
#include "dlist.h"
#include "scheduler.h"

static uint32_t jid = 0;

void scheduler_init(scheduler_t* scheduler) {
    dlist_init(&(*scheduler).ready_queue);
    dlist_init(&(*scheduler).waiting_queue);
    scheduler->running_job = NULL;
}

uint32_t scheduler_next_jid(void) {
    return jid++;
}

void scheduler_run(scheduler_t* scheduler) {
    while (dlist_length(&(*scheduler).waiting_queue) > 0 ||
           dlist_is_empty(&(*scheduler).ready_queue) > 0) {
        while (dlist_length(&(*scheduler).ready_queue) > 0) {
            job_t* next_job = (job_t*)dlist_take_first(&(*scheduler).ready_queue);
            next_job->mode = JOB_MODE_RUNNING;
            scheduler->running_job = next_job;

            interpreter_result_t result = INTERPRETER_RESULT_HALT;

            switch (result) {
            case INTERPRETER_RESULT_HALT:
                printf("Job %d halted: ... \n", next_job->jid);
                break;
            case INTERPRETER_RESULT_RECV:
                next_job->mode = JOB_MODE_WAITING;
                dlist_insert_last(&(*scheduler).waiting_queue, next_job);
                break;
            case INTERPRETER_RESULT_TIMEOUT:
                next_job->mode = JOB_MODE_READY;
                dlist_insert_last(&(*scheduler).ready_queue, next_job);
                break;
            case INTERPRETER_RESULT_EXIT:
                return;
            }
        }
        sleep(scheduler->time_slice / 1000);
    }
}

void scheduler_spawn(scheduler_t* scheduler, job_t* job) {
    job->mode = JOB_MODE_READY;
    dlist_insert_last(&(*scheduler).ready_queue, job);
}

void scheduler_send_message(scheduler_t* scheduler, uint32_t jid,
                            int64_t value) {
    job_t* job = scheduler_find_job(scheduler, jid);
    if (job != NULL) {
        //message_box_enqueue(&job->message_box, value); // FIXME
        if (job->mode == JOB_MODE_WAITING) {
            remove_from_waiting_queue(scheduler, job->jid);
            job->mode = JOB_MODE_READY;
            dlist_insert_last(&(*scheduler).ready_queue, job);
        }
    } else {
        assert(false);
    }
}

job_t* scheduler_find_job(scheduler_t* scheduler, uint32_t jid) {
    if (scheduler->running_job != NULL && scheduler->running_job->jid == jid) {
        return scheduler->running_job;
    }
    dlist_iter_t iter;
    dlist_iter_init(&iter, &(*scheduler).ready_queue);
    while(!dlist_iter_end(&iter)) {
        job_t* job = (job_t*)dlist_iter_current(&iter);
        if (job->jid == jid) {
            return job;
        } else {
            dlist_iter_next(&iter);
        }
    }
    dlist_iter_init(&iter, &(*scheduler).waiting_queue);
    while(!dlist_iter_end(&iter)) {
        job_t* job = (job_t*)dlist_iter_current(&iter);
        if (job->jid == jid) {
            return job;
        } else {
            dlist_iter_next(&iter);
        }
    }
    return NULL;
}
