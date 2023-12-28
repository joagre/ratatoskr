#include <stdlib.h>
#include "job.h"

job_t* job_new(job_t* parent_job, uint32_t jid, vm_address_t pc,
	       vm_stack_value_t* parameters, vm_arity_t arity) {
    LOG_ASSERT(arity + 1 < NUMBER_OF_REGISTERS,
               "Register arity %d > %d", arity, NUMBER_OF_REGISTERS - 1);

    job_t* job = malloc(sizeof(job_t));
    job->jid = jid;
    job->mode = JOB_MODE_INIT;
    job->pc = pc;

    // Initialize call stack
    call_stack_init(&job->call_stack);
    call_stack_push(&job->call_stack, 0); // Sentinel return address
    call_stack_push(&job->call_stack, 0); // Sentinel frame pointer

    // Initialize mailbox
    mailbox_init(&job->mailbox);

    // Initialize registers
    memset(job->registers, 0, sizeof(vm_stack_value_t) * NUMBER_OF_REGISTERS);
    if (parameters == NULL) {
	for (vm_arity_t i = 0; i < arity; i++) {
	    job->registers[i + 1] = parent_job->registers[i + 1];
	}
    } else {
	for (vm_arity_t i = 0; i < arity; i++) {
	    job->registers[i + 1] = parameters[i];
	}
    }

    return job;
}

void job_free(job_t* job) {
    call_stack_clear(&job->call_stack);
    mailbox_clear(&job->mailbox);
    free(job);
}
