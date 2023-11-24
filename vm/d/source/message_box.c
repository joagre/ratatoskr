#include "dlist.h"
#include "message_box.h"

bool message_box_init(message_box_t* message_box) {
    dlist_init(&(*scheduler).messages);
    return true;
}





// Constructor-like function to initialize MessageBox
MessageBox* MessageBox_create() {
    MessageBox* mb = (MessageBox*) malloc(sizeof(MessageBox));
    if (!mb) {
        // Handle memory allocation failure
        exit(1); // Or return NULL depending on your desired error handling strategy
    }
    mb->messageBox = dlist_create();
    mb->length = 0;
    return mb;
}

// Function to dequeue a message from the MessageBox
long MessageBox_dequeue(MessageBox* mb) {
    if (!mb->length) {
        // Handle underflow situation, e.g., trying to dequeue from an empty MessageBox
        exit(1); // Or return a sentinel value or set an error code
    }
    dlist_node* front_node = dlist_front(mb->messageBox);
    long message = *(long*)front_node->data;
    dlist_remove_front(mb->messageBox);
    mb->length--;
    return message;
}

// Function to enqueue a message to the MessageBox
void MessageBox_enqueue(MessageBox* mb, long message) {
    dlist_insert_back(mb->messageBox, &message, sizeof(long));
    mb->length++;
}

// Optionally: Destructor-like function to destroy MessageBox and free memory
void MessageBox_destroy(MessageBox* mb) {
    dlist_destroy(mb->messageBox);
    free(mb);
}
