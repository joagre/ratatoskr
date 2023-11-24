#ifndef __JOB_H__
#define __JOB_H__

#include <stdint.h>

typedef enum {
    JOB_MODE_READY,
    JOB_MODE_RUNNING,
    JOB_MODE_WAITING,
} job_mode_t;

typedef struct {
    job_mode_t mode;
    uint32_t jid;
} job_t;

#endif
