#define MUTE_LOG_DEBUG 1

#include <stdlib.h>
#include "ready_queue.h"

void ready_queue_init(ready_queue_t* queue) {
    dlist_init(queue);
}

void ready_queue_clear(ready_queue_t* queue) {
    dlist_iter_t iter;
    dlist_iter_init(&iter, queue);
    while(!dlist_iter_end(&iter)) {
        job_t* job = dlist_iter_current(&iter);
        dlist_iter_remove(&iter);
        job_free(job);
    }
}

job_t* ready_queue_dequeue(ready_queue_t* queue) {
    ready_queue_link_t* link = dlist_take_first(queue);
    job_t* job = link->job;
    free(link);
    return job;
}

void ready_queue_enqueue(ready_queue_t* queue, job_t* job) {
    ready_queue_link_t* link = malloc(sizeof(ready_queue_link_t));
    link->job = job;
    dlist_insert_last(queue, link);
}

bool ready_queue_is_empty(ready_queue_t* queue) {
    return dlist_is_empty(queue);
}

job_t* ready_queue_find(ready_queue_t* queue, uint32_t jid) {
    dlist_iter_t iter;
    dlist_iter_init(&iter, queue);
    while(!dlist_iter_end(&iter)) {
        job_t* current_job =
            ((ready_queue_link_t*)dlist_iter_current(&iter))->job;
        if (current_job->jid == jid) {
            return current_job;
        } else {
            dlist_iter_next(&iter);
        }
    }
    return NULL;
}
