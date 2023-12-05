//#define MUTE_LOG_DEBUG 1

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
#define DEFAULT_INTERPRETER_MODE INTERPRETER_MODE_STACK

void usage(char* name);

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
    interpreter_mode_t mode = DEFAULT_INTERPRETER_MODE;

    // Parse command line options
    struct option longopts[] =
        {
            {
                .name = "time-slice",
                .has_arg = required_argument,
                .flag = 0,
                .val = 't'
            },
            {
                .name = "check-after",
                .has_arg = required_argument,
                .flag = 0,
                .val = 'c'
            },
            {
                .name = "load-path",
                .has_arg = required_argument,
                .flag = 0,
                .val = 'l'
            },
            {
                .name = "interpreter",
                .has_arg = required_argument,
                .flag = 0,
                .val = 'i'
            },
            {
                .name = "help",
                .has_arg = no_argument,
                .flag = 0,
                .val = 'h'
            },
            {0, 0, 0, 0}
        };
    int longopt;
    int longindex = 0;
    satie_error_t error;
    while ((longopt = getopt_long(argc, argv, "t:c:l:i:", longopts,
                                  &longindex)) != -1) {
        switch (longopt) {
        case 't': {
            time_slice = string_to_long(optarg, &error);
            if (error.failed) {
                usage(basename(argv[0]));
            }
            break;
        }
        case 'c': {
            check_after = string_to_long(optarg, &error);
            if (error.failed) {
                usage(basename(argv[0]));
            }
            break;
        }
        case 'l':
            load_path = optarg;
            break;
        case 'i':
            if (strcmp(optarg, "stack") == 0) {
                mode = INTERPRETER_MODE_STACK;
            } else if (strcmp(optarg, "register") == 0) {
                mode = INTERPRETER_MODE_REGISTER;
            } else {
                usage(basename(argv[0]));
            }
            break;
        case 'h':
            usage(basename(argv[0]));
            break;
        default:
            usage(basename(argv[0]));
        }
    }

    if (argc < 3) {
        usage(basename(argv[0]));
    }

    // Parse positional arguments
    char* module_name = argv[optind];
    uint32_t label = string_to_long(argv[optind + 1], &error);
    if (error.failed) {
        usage(basename(argv[0]));
    }
    long parameters[argc - optind];
    for (int j = 0, i = optind + 2; i < argc; i++) {
        parameters[j++] = string_to_long(argv[i], &error);
        if (error.failed) {
            usage(basename(argv[0]));
        }
    }

    LOG_DEBUG("check_after = %d", check_after);
    LOG_DEBUG("load_path = %s", load_path);
    LOG_DEBUG("time_slice = %d", time_slice);
    LOG_DEBUG("module_name = %s", module_name);
    LOG_DEBUG("label = %d", label);
    for (int i = 0; i < argc - optind - 2; i++) {
        LOG_DEBUG("parameter = %d", parameters[i]);
    }

    // Prepare loader
    loader_t loader;
    loader_init(&loader, load_path);

    // Prepare interpreter
    interpreter_t interpreter;
    interpreter_init(&interpreter, mode);

    // Prepare scheduler
    scheduler_t scheduler;
    scheduler_init(&scheduler, time_slice, check_after, &loader, &interpreter);

    // Spawn job according to command line arguments
    interpreter_mspawn(&scheduler, module_name, label, parameters,
                       argc - optind - 2, &error);
    if (error.failed) {
        satie_print_error(&error);
        return SPAWN_ERROR;
    }

    // Start scheduler
    scheduler_run(&scheduler);

    /*
    loader_load_module(&loader, module_name, &error);
    if (error.failed) {
        satie_print_error(&error);
        return SUCCESS;
    }
    loader_load_module(&loader, module_name, &error);
    if (error.failed) {
        satie_print_error(&error);
        return SUCCESS;
    }

    pretty_print(&loader);
    */

    return SUCCESS;
}

void usage(char* name) {
    fprintf(stderr,
            "Usage: %s [options] <module> <label> [<parameter> ...]\n"
            "Options:\n"
            "  -c <instructions>, --check-after=<instructions>\n"
            "    Check time slice timeout each number of <instructions> (%d)\n"
            "  -h, --help\n"
            "    Print this message and exit\n"
            "  -i, interpreter-mode <mode>\n"
            "    Start interpreter in 'stack' or 'register' <mode> (%s)\n"
            "  -l <directory>, --load-path=<directory>\n"
            "    Load POSM files from <directory> (%s)\n"
            "  -t <milli-seconds>, --time-slice=<milli-seconds>\n"
            "    <milli-seconds> spent by each job before context switch (%d) "
            "ms)\n",
            name, DEFAULT_CHECK_AFTER,
            DEFAULT_INTERPRETER_MODE ==
                INTERPRETER_MODE_STACK ? "stack" : "register",
            DEFAULT_LOAD_PATH, DEFAULT_TIME_SLICE);
    exit(PARAMETER_ERROR);
}
