#ifndef SA_READY_QUEUE_H
#define SA_READY_QUEUE_H

#include <dlist.h>
#include "job.h"

typedef struct {
    dlink_t link;
    job_t* job;
} ready_queue_link_t;

typedef dlist_t ready_queue_t;

void ready_queue_init(ready_queue_t* queue);
void ready_queue_clear(ready_queue_t* queue);
job_t* ready_queue_dequeue(ready_queue_t* queue);
void ready_queue_enqueue(ready_queue_t* queue, job_t* job);
bool ready_queue_is_empty(ready_queue_t* queue);
job_t* ready_queue_find(ready_queue_t* queue, uint32_t jid);

#endif
