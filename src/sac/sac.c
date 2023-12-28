#define MUTE_LOG_DEBUG 1

#include <stdlib.h>
#include <getopt.h>
#include <log.h>
#include <utils.h>
#include "compiler.h"

#define SUCCESS 0
#define PARAMETER_ERROR 1
#define COMPILE_ERROR 1

#define DEFAULT_OUTPUT_DIRECTORY "./"

// Forward declarations of local functions (alphabetical order)
static void usage(char* name);

int main(int argc, char* argv[]) {
    char* output_directory = DEFAULT_OUTPUT_DIRECTORY;

    // Parse command line options
    struct option longopts[] =
        {
            {
                .name = "output",
                .has_arg = required_argument,
                .flag = 0,
                .val = 'o'
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

    while ((longopt = getopt_long(argc, argv, "o:h", longopts,
                                  &longindex)) != -1) {
        switch (longopt) {
	    case 'h':
		usage(basename(argv[0]));
		break;
	    case 'o':
		output_directory = optarg;
		break;
	    default:
		usage(basename(argv[0]));
        }
    }

    if (argc - optind != 1) {
        usage(basename(argv[0]));
    }

    // Parse positional arguments
    char* filename = argv[optind];
    if (!is_valid_extension(filename, "sai")) {
        usage(basename(argv[0]));
    }

    LOG_DEBUG("filename = %s", filename);
    LOG_DEBUG("output_directory = %s", output_directory);

    compiler_t compiler;
    compiler_init(&compiler);
    satie_error_t error;
    compiler_compile(&compiler, filename, output_directory, &error);
    if (error.failed) {
        compiler_clear(&compiler);
	LOG_SATIE_ERROR(LOG_LEVEL_ERROR, &error);
        return COMPILE_ERROR;
    } else {
        compiler_clear(&compiler);
        return SUCCESS;
    }
}

//
// Local functions (alphabetical order)
//

static void usage(char* name) {
    fprintf(stderr,
            "Usage: %s [options] <file>.sai\n"
            "Options:\n"
            "  -o <directory>\n"
            "    Write <file>.sab to <directory> (%s)\n"
            "  -h, --help\n"
            "    Print this message and exit\n",
            name, DEFAULT_OUTPUT_DIRECTORY);
    exit(PARAMETER_ERROR);
}
