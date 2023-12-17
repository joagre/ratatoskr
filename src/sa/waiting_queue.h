#ifndef __WAITING_QUEUE_H__
#define __WAITING_QUEUE_H__

#include <dlist.h>
#include "job.h"

typedef struct {
    dlink_t link;
    job_t* job;
} waiting_queue_link_t;

typedef dlist_t waiting_queue_t;

void waiting_queue_init(waiting_queue_t* queue);
void waiting_queue_free(waiting_queue_t* queue);
job_t* waiting_queue_dequeue(waiting_queue_t* queue);
void waiting_queue_enqueue(waiting_queue_t* queue, job_t* job);
bool waiting_queue_is_empty(waiting_queue_t* queue);
void waiting_queue_remove(waiting_queue_t* queue, job_t* job);
job_t* waiting_queue_find(waiting_queue_t* queue, uint32_t jid);

#endif
