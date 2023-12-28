#ifndef __JOB_H__
#define __JOB_H__

#include <stdint.h>
#include "vm.h"
#include "call_stack.h"
#include "mailbox.h"

typedef enum {
    JOB_MODE_INIT,
    JOB_MODE_RUNNING,
    JOB_MODE_READY,
    JOB_MODE_WAITING
} job_mode_t;

typedef struct {
    uint32_t jid;
    job_mode_t mode;
    vm_address_t pc;
    call_stack_t call_stack;
    mailbox_t mailbox;
    vm_stack_value_t registers[NUMBER_OF_REGISTERS];
} job_t;

job_t* job_new(job_t* parent_job, uint32_t jid, vm_address_t pc,
	       vm_stack_value_t* parameters, vm_arity_t arity);
void job_free(job_t* job);

#endif
