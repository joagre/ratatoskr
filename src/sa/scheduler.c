#include <log.h>
#include <utils.h>
#include "scheduler.h"

// Used to generate unique job IDs
static uint32_t jid = 0;

// Forward declarations of local functions (alphabetical order)
static job_t* find_job(scheduler_t* scheduler, uint32_t jid);

void scheduler_init(scheduler_t* scheduler, loader_t* loader,
                    interpreter_t* interpreter, uint32_t time_slice,
                    uint16_t check_after) {
    ready_queue_init(&scheduler->ready_queue);
    waiting_queue_init(&scheduler->waiting_queue);
    scheduler->loader = loader;
    scheduler->interpreter = interpreter;
    scheduler->time_slice = time_slice;
    scheduler->check_after = check_after;
    scheduler->running_job = NULL;
}

void scheduler_clear(scheduler_t* scheduler) {
    ready_queue_clear(&scheduler->ready_queue);
    waiting_queue_clear(&scheduler->waiting_queue);
    if (scheduler->running_job != NULL) {
        job_free(scheduler->running_job);
    }
}

uint32_t scheduler_next_jid(void) {
    return jid++;
}

void scheduler_run(scheduler_t *scheduler) {
    while (!waiting_queue_is_empty(&scheduler->waiting_queue) ||
           !ready_queue_is_empty(&scheduler->ready_queue)) {
        while (!ready_queue_is_empty(&scheduler->ready_queue)) {
            job_t* next_job = ready_queue_dequeue(&scheduler->ready_queue);
            next_job->mode = JOB_MODE_RUNNING;
            scheduler->running_job = next_job;
            interpreter_result_t result = interpreter_run(scheduler);
            switch (result) {
		case INTERPRETER_RESULT_HALT:
		    LOG_DEBUG("Job %d halt (r0 = %d)",
			      scheduler->running_job->jid,
			      scheduler->running_job->registers[0]);
		    break;
		case INTERPRETER_RESULT_RECV:
		    scheduler->running_job->mode = JOB_MODE_WAITING;
		    waiting_queue_enqueue(&scheduler->waiting_queue,
					  scheduler->running_job);
		    break;
		case INTERPRETER_RESULT_TIMEOUT:
		    scheduler->running_job->mode = JOB_MODE_READY;
		    ready_queue_enqueue(&scheduler->ready_queue,
					scheduler->running_job);
		    break;
		case INTERPRETER_RESULT_EXIT:
		    return;
            }
        }
        sleep_ms(scheduler->time_slice);
    }
}

void scheduler_spawn(scheduler_t* scheduler, job_t* job) {
    job->mode = JOB_MODE_READY;
    ready_queue_enqueue(&scheduler->ready_queue, job);
}

void scheduler_send_message(scheduler_t* scheduler, uint32_t jid,
                            vm_stack_value_t value) {
    job_t* job = find_job(scheduler, jid);
    if (job == NULL) {
        LOG_ABORT("Job %d not found", jid);
    } else {
        mailbox_enqueue(&job->mailbox, value);
        if (job->mode == JOB_MODE_WAITING) {
            waiting_queue_remove(&scheduler->waiting_queue, job);
            job->mode = JOB_MODE_READY;
            ready_queue_enqueue(&scheduler->ready_queue, job);
        }
    }
}

//
// Local functions (alphabetical order)
//

static job_t* find_job(scheduler_t* scheduler, uint32_t jid) {
    if (scheduler->running_job != NULL && scheduler->running_job->jid == jid) {
        return scheduler->running_job;
    }
    job_t* job = ready_queue_find(&scheduler->ready_queue, jid);
    if (job == NULL) {
        job = waiting_queue_find(&scheduler->waiting_queue, jid);
    }
    return job;
}
