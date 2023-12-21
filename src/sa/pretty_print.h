#ifndef __PRETTY_PRINT_H__
#define __PRETTY_PRINT_H__

#include <stdint.h>
#include "static_data.h"

uint32_t print_instruction(uint8_t* bytes, static_data_t* static_data);

#endif
