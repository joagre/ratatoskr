#include <stdint.h>
#include <stdlib.h>
#include <stdint.h>
#include <stdio.h>
#include <getopt.h>
#include <stdbool.h>
#include <libgen.h>
#include <errno.h>
#include "loader.h"
#include "satie.h"
#include "return_types.h"
#include "log.h"

int main(int argc, char* argv[]) {
    uint16_t check_after = DEFAULT_CHECK_AFTER;
    char* load_path = DEFAULT_LOAD_PATH;
    uint32_t time_slice = DEFAULT_TIME_SLICE;

    struct option long_options[] = {
        {"time-slice", required_argument, 0, 't'},
        {"check-after", required_argument, 0, 'c'},
        {"load-path", required_argument, 0, 'l'},
        {0, 0, 0, 0}
    };

    int opt, option_index = 0;
    while ((opt = getopt_long(argc, argv, "t:c:l:i:", long_options,
                              &option_index)) != -1) {
        switch (opt) {
        case 't': {
            long_result_t result = string_to_long(optarg);
            if (!result.success) {
                usage(basename(argv[0]));
            }
            time_slice = result.value;
            break;
        }
        case 'c': {
            long_result_t result = string_to_long(optarg);
            if (!result.success) {
                usage(basename(argv[0]));
            }
            check_after = result.value;
            break;
        }
        case 'l':
            load_path = optarg;
            break;
        default:
            usage(basename(argv[0]));
        }
    }

    if (argc < 3) {
        usage(basename(argv[0]));
    }

    const char* module_name = argv[optind];

    long_result_t result = string_to_long(argv[optind + 1]);
    if (!result.success) {
        usage(basename(argv[0]));
    }
    uint32_t label = result.value;

    long parameters[argc - optind];
    for (int j = 0, i = optind + 2; i < argc; i++) {
        long_result_t result = string_to_long(argv[i]);
        if (!result.success) {
            usage(basename(argv[0]));
        }
        parameters[j++] = result.value;
    }

    VM_LOG(LOG_LEVEL_DEBUG, "check_after = %d", check_after);
    VM_LOG(LOG_LEVEL_DEBUG, "load_path = %s", load_path);
    VM_LOG(LOG_LEVEL_DEBUG, "time_slice = %d", time_slice);
    VM_LOG(LOG_LEVEL_DEBUG, "module_name = %s", module_name);
    VM_LOG(LOG_LEVEL_DEBUG, "label = %d", label);
    for (int i = 0; i < argc - optind - 2; i++) {
        VM_LOG(LOG_LEVEL_DEBUG, "parameter = %d", parameters[i]);
    }

    return SUCCESS;
}

void usage(const char* name) {
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
            name, DEFAULT_CHECK_AFTER, DEFAULT_LOAD_PATH, DEFAULT_TIME_SLICE);
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
