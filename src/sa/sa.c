#define MUTE_LOG_DEBUG 1

#include <stdio.h>
#include <stdlib.h>
#include <getopt.h>
#include <libgen.h>
#include "log.h"
#include "loader.h"
#include "mailbox.h"
#include "call_stack.h"
#include "util.h"
#include "interpreter.h"

#define SUCCESS 0
#define PARAMETER_ERROR 1
#define SPAWN_ERROR 2

#define DEFAULT_CHECK_AFTER 100
#define DEFAULT_LOAD_PATH "./"
#define DEFAULT_TIME_SLICE 25

// Forward declarations of local functions (alphabetical order)
static void usage(char* name);

int main(int argc, char* argv[]) {
#ifdef UNITTEST
    module_unit_test();
    loader_unit_test();
    mailbox_unit_test();
    call_stack_unit_test();
#endif

    uint16_t check_after = DEFAULT_CHECK_AFTER;
    char* load_path = DEFAULT_LOAD_PATH;
    uint32_t time_slice = DEFAULT_TIME_SLICE;

    // Parse command line options
    struct option longopts[] =
        {
            {
                .name = "check-after",
                .has_arg = required_argument,
                .flag = 0,
                .val = 'c'
            },
            {
                .name = "help",
                .has_arg = no_argument,
                .flag = 0,
                .val = 'h'
            },
            {
                .name = "interpreter",
                .has_arg = required_argument,
                .flag = 0,
                .val = 'i'
            },
            {
                .name = "load-path",
                .has_arg = required_argument,
                .flag = 0,
                .val = 'l'
            },
            {
                .name = "time-slice",
                .has_arg = required_argument,
                .flag = 0,
                .val = 't'
            },
            {0, 0, 0, 0}
        };

    satie_error_t error;
    int longopt;
    int longindex = 0;

    while ((longopt = getopt_long(argc, argv, "c:hl:t:", longopts,
                                  &longindex)) != -1) {
        switch (longopt) {
        case 'c': {
            check_after = string_to_long(optarg, &error);
            if (error.failed) {
                usage(basename(argv[0]));
            }
            break;
        }
        case 'h':
            usage(basename(argv[0]));
            break;
        case 'l':
            load_path = optarg;
            break;
        case 't': {
            time_slice = string_to_long(optarg, &error);
            if (error.failed) {
                usage(basename(argv[0]));
            }
            break;
        }
        default:
            usage(basename(argv[0]));
        }
    }

    if (argc - optind < 2) {
        usage(basename(argv[0]));
    }

    // Parse positional arguments
    char* module_name = argv[optind];
    uint32_t label = string_to_long(argv[optind + 1], &error);
    if (error.failed) {
        usage(basename(argv[0]));
    }
    vm_stack_value_t parameters[argc - optind];
    for (int j = 0, i = optind + 2; i < argc; i++) {
        parameters[j++] = string_to_long(argv[i], &error);
        if (error.failed) {
            usage(basename(argv[0]));
        }
    }
    size_t number_of_parameters = argc - optind - 2;

    LOG_DEBUG("check_after = %d", check_after);
    LOG_DEBUG("load_path = %s", load_path);
    LOG_DEBUG("time_slice = %d", time_slice);
    LOG_DEBUG("module_name = %s", module_name);
    LOG_DEBUG("label = %d", label);
    for (size_t i = 0; i < number_of_parameters; i++) {
        LOG_DEBUG("parameter = %d", parameters[i]);
    }

    // Prepare loader
    loader_t loader;
    loader_init(&loader, load_path);

    // Prepare interpreter
    interpreter_t interpreter;
    interpreter_init(&interpreter, 0);

    // Prepare scheduler
    scheduler_t scheduler;
    scheduler_init(&scheduler, &loader, &interpreter, time_slice, check_after);

    // Spawn job according to command line arguments
    interpreter_mspawn(&scheduler, module_name, label, parameters,
                       number_of_parameters, &error);
    if (error.failed) {
        satie_print_error(&error);
        return SPAWN_ERROR;
    }

    // Start scheduler
    scheduler_run(&scheduler);

    fprintf(stderr, "Scheduler finished\n");

    return SUCCESS;
}

//
// Local functions (alphabetical order)
//

static void usage(char* name) {
    fprintf(stderr,
            "Usage: %s [options] <module> <label> [<parameter> ...]\n"
            "Options:\n"
            "  -c <instructions>, --check-after=<instructions>\n"
            "    Check time slice timeout each number of <instructions> (%d)\n"
            "  -h, --help\n"
            "    Print this message and exit\n"
            "  -l <directory>, --load-path=<directory>\n"
            "    Load POSM files from <directory> (%s)\n"
            "  -t <milli-seconds>, --time-slice=<milli-seconds>\n"
            "    <milli-seconds> spent by each job before context switch (%d) "
            "ms)\n",
            name, DEFAULT_CHECK_AFTER,
            DEFAULT_LOAD_PATH, DEFAULT_TIME_SLICE);
    exit(PARAMETER_ERROR);
}
