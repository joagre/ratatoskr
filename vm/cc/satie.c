#include <stdint.h>
#include <stdlib.h>
#include <stdint.h>
#include <stdio.h>
#include <getopt.h>
#include <stdbool.h>
#include <libgen.h>
#include <errno.h>
#include "vm.h"
#include "satie.h"
#include "return_types.h"

int main(int argc, char* argv[]) {
    uint16_t check_after = DEFAULT_CHECK_AFTER;
    char* load_path = DEFAULT_LOAD_PATH;
    uint32_t time_slice = DEFAULT_TIME_SLICE;

    struct option long_options[] = {
        {"time-slice", required_argument, 0, 't'},
        {"check-after", required_argument, 0, 'c'},
        {"load-path", required_argument, 0, 'l'},
        {"interpreter-mode", required_argument, 0, 'i'},
        {0, 0, 0, 0}
    };

    int opt, option_index = 0;
    while ((opt = getopt_long(argc, argv, "t:c:l:i:", long_options,
                              &option_index)) != -1) {
        switch (opt) {
        case 't': {
            long_result_t result = string_to_long(optarg);
            if (!result.success) {
                usage(basename(argv[0]), check_after, load_path, time_slice);
            }
            time_slice = result.value;
            break;
        }
        case 'c': {
            long_result_t result = string_to_long(optarg);
            if (!result.success) {
                usage(basename(argv[0]), check_after, load_path, time_slice);
            }
            check_after = result.value;
            break;
        }
        case 'l':
            load_path = optarg;
            break;
        default:
            usage(basename(argv[0]), check_after, load_path, time_slice);
        }
    }

    printf("Time Slice: %d\n", time_slice);
    printf("Check After: %d\n", check_after);
    printf("Load Path: %s\n", load_path);
    //printf("Interpreter Mode: %d\n", interpreterMode);

    return SUCCESS;
}

void usage(const char* name, uint16_t check_after, const char* load_path,
           uint32_t time_slice) {
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
            "    <milli-seconds> spent by each job before context switch (%d) "
            "ms)\n",
            name, check_after, load_path, time_slice);
    exit(PARAMETER_ERROR);
}

long_result_t string_to_long(const char* string) {
    errno = 0;
    char *endptr;
    long value = strtol(string, &endptr, 10);
    if (errno != 0 || *endptr != '\0' || optarg == endptr) {
        return (long_result_t){ .success = false };
    } else {
        return (long_result_t){ .success = true, .value = value };
    }
}
