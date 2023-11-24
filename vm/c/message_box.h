#ifndef __MESSAGE_BOX_H__
#define __MESSAGE_BOX_H__

#include <stdint.h>
#include "dlist.h"

typedef struct {
    dlist_t messages;
} message_box_t;

void message_box_init(message_box_t* message_box);
void message_box_enqueue(message_box_t* message_box, int32_t message);
int32_t message_box_dequeue(message_box_t* message_box);

#endif
