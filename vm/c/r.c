#include <stdio.h>
#include "scheduler.h"

int main(void) {
    scheduler_t scheduler;
    scheduler_init(&scheduler);
    printf("Hello, World!\n");
    return 0;
}
