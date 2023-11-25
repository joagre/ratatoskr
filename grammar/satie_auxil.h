#ifndef SATIE_AUXIL_H
#define SATIE_AUXIL_H

static int LINE = 1;

typedef struct {
    int line;
} satie_auxil_t;

static void panic(const char *fmt, ...) {
    va_list args;
    va_start(args, fmt);
    fprintf(stderr, "\033[31mError:\033[0m ");
    vfprintf(stderr, fmt, args);
    fprintf(stderr, "\n");
    va_end(args);
    exit(1);
}

static void satie_error(satie_auxil_t* auxil) {
    panic("Bailing out near line %d\n", LINE);
    exit(1);
}

static int satie_getchar(satie_auxil_t* _auxil) {
    int c = getchar();
    if (c == '\n') {
        LINE++;
    }
    return c;
}

#endif
