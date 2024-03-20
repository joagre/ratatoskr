#ifndef UTILS_BITS_H
#define UTILS_BITS_H

#define IS_BIT_SET(value, pos) ((value) & (1 << (pos)))
#define SET_BIT(value, pos) ((value) | (1 << (pos)))

#endif
