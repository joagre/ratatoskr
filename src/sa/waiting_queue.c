#define MUTE_LOG_DEBUG 1

#include <stdlib.h>
#include "waiting_queue.h"
#include "log.h"

void waiting_queue_init(waiting_queue_t* queue) {
    dlist_init(queue);
}

void waiting_queue_clear(waiting_queue_t* queue) {
    dlist_iter_t iter;
    dlist_iter_init(&iter, queue);
    while(!dlist_iter_end(&iter)) {
        job_t* job = dlist_iter_current(&iter);
        dlist_iter_remove(&iter);
        job_free(job);
    }
}

job_t* waiting_queue_dequeue(waiting_queue_t* queue) {
    waiting_queue_link_t* link = dlist_take_first(queue);
    job_t* job = link->job;
    free(link);
    return job;
}

void waiting_queue_enqueue(waiting_queue_t* queue, job_t* job) {
    waiting_queue_link_t* link = malloc(sizeof(waiting_queue_link_t));
    link->job = job;
    dlist_insert_last(queue, link);
}

bool waiting_queue_is_empty(waiting_queue_t* queue) {
    return dlist_is_empty(queue);
}

void waiting_queue_remove(waiting_queue_t* queue, job_t* job) {
    dlist_iter_t iter;
    dlist_iter_init(&iter, queue);
    while(!dlist_iter_end(&iter)) {
        job_t* current_job =
            ((waiting_queue_link_t*)dlist_iter_current(&iter))->job;

        if (current_job->jid == job->jid) {
            dlist_iter_remove(&iter);
            return;
        } else {
            dlist_iter_next(&iter);
        }
    }
    LOG_ABORT("Job is missing in waiting queue");
}

job_t* waiting_queue_find(waiting_queue_t* queue, uint32_t jid) {
    dlist_iter_t iter;
    dlist_iter_init(&iter, queue);
    while(!dlist_iter_end(&iter)) {
        job_t* current_job =
            ((waiting_queue_link_t*)dlist_iter_current(&iter))->job;
        if (current_job->jid == jid) {
            return current_job;
        } else {
            dlist_iter_next(&iter);
        }
    }
    return NULL;
}
