#include <stdlib.h>
#include "job.h"

job_t* new_job(uint32_t jid, vm_address_t pc,
               call_stack_array_t* stack_array) {
    job_t* job = malloc(sizeof(job_t));
    job->jid = jid;
    job->mode = JOB_MODE_INIT;
    job->pc = pc;
    data_stack_init(&job->data_stack);
    call_stack_init(&job->call_stack, stack_array, &job->data_stack);
    mailbox_init(&job->mailbox);
    return job;
}

void free_job(job_t* job) {
    data_stack_free(&job->data_stack);
    call_stack_free(&job->call_stack);
    mailbox_free(&job->mailbox);
    free(job);
}
