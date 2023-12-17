#ifndef __MAILBOX_H__
#define __MAILBOX_H__

#include "clib/dlist.h"
#include "vm.h"

typedef struct {
    dlink_t link;
    vm_stack_value_t value;
} mailbox_link_t;

typedef dlist_t mailbox_t;

void mailbox_init(mailbox_t* mailbox);
void mailbox_free(mailbox_t* mailbox);
vm_stack_value_t mailbox_dequeue(mailbox_t* mailbox);
void mailbox_enqueue(mailbox_t* mailbox, vm_stack_value_t value);
bool mailbox_is_empty(mailbox_t* mailbox);
void mailbox_unit_test(void);

#endif
