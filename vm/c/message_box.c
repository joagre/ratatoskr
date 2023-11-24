#include <stdint.h>
#include <assert.h>
#include "dlist.h"
#include "message_box.h"

void message_box_init(message_box_t* message_box) {
    dlist_init(&(*message_box).messages);
}

void message_box_enqueue(message_box_t* message_box, int32_t message) {
    dlist_insert_first(&(*message_box).messages, &message);
}

int32_t message_box_dequeue(message_box_t* message_box) {
    assert((*message_box).messages.length != 0);
    int32_t* message = (int32_t*)dlist_take_last(&(*message_box).messages);
    return *message;
}
