#ifndef SATIE_AUXIL_H
#define SATIE_AUXIL_H

typedef struct {
    int line;
} satie_auxil_t;

static void panic(const char *fmt, ...) {
    va_list args;
    va_start(args, fmt);
    fprintf(stderr, "Syntax error: ");
    vfprintf(stderr, fmt, args);
    fprintf(stderr, "\n");
    va_end(args);
    exit(1);
}

#endif
