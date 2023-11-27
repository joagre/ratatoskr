#include <stdint.h>
#include <stdlib.h>
#include <stdint.h>
#include <stdio.h>
#include <libgen.h>
#include "vm.h"

#define SUCCESS 0
#define PARAMETER_ERROR 1

void usage(char* name, uint16_t check_after, char* load_path, uint32_t time_slice) {
    fprintf(stderr,
            "Usage: %s [options] <module> <label> [<parameter> ...]\n"
            "Options:\n"
            "  -c <instructions>, --check-after=<instructions>\n"
            "    Check time slice timeout each number of <instructions> (%d)\n"
            "  -h, --help\n"
            "    Print this message and exit\n"
            "  -i, interpreter-mode <mode>\n"
            "    Start interpreter in 'stack' or 'register' <mode>\n"
            "  -l <directory>, --load-path=<directory>\n"
            "    Load POSM files from <directory> (%s)\n"
            "  -t <milli-seconds>, --time-slice=<milli-seconds>\n"
            "    <milli-seconds> spent by each job before context switch (%d) ms)\n",
            name, check_after, load_path, time_slice);
    exit(PARAMETER_ERROR);
}

int main(int, char* argv[]) {
    uint16_t check_after = 100;
    char* load_path = "./";
    uint32_t time_slice = 25;

    usage(basename(argv[0]), check_after, load_path, time_slice);

    return SUCCESS;
}
