#define MUTE_LOG_DEBUG 1

#include <stdlib.h>
#include "mailbox.h"
#include "log.h"

void mailbox_init(mailbox_t* mailbox) {
    dlist_init(mailbox);
}

void mailbox_free(mailbox_t* mailbox) {
    dlist_iter_t iter;
    dlist_iter_init(&iter, mailbox);
    while(!dlist_iter_end(&iter)) {
        mailbox_link_t* link = dlist_iter_current(&iter);
        dlist_iter_remove(&iter);
        free(link);
    }
}

vm_stack_value_t mailbox_dequeue(mailbox_t* mailbox) {
    mailbox_link_t* link = dlist_take_first(mailbox);
    vm_stack_value_t value = link->value;
    free(link);
    return value;
}

void mailbox_enqueue(mailbox_t* mailbox, vm_stack_value_t value) {
    mailbox_link_t* link = malloc(sizeof(mailbox_link_t));
    link->value = value;
    dlist_insert_last(mailbox, link);
}

bool mailbox_is_empty(mailbox_t* mailbox) {
    return dlist_is_empty(mailbox);
}

//
// Unit test
//

void mailbox_unit_test(void) {
    mailbox_t mailbox;
    // init
    mailbox_init(&mailbox);

    // enqueue and dequeue
    mailbox_enqueue(&mailbox, 1);
    LOG_ASSERT(mailbox_dequeue(&mailbox) == 1, "mailbox_dequeue failed");

    // is_empty
    LOG_ASSERT(mailbox_is_empty(&mailbox), "mailbox_is_empty failed");
    mailbox_free(&mailbox);

    LOG_INFO("Unit test passed");
}
