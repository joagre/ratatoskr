#ifndef BITS_H
#define BITS_H

#define IS_BIT_SET(value, pos) ((value) & (1 << (pos)))
#define SET_BIT(value, pos) ((value) | (1 << (pos)))

#endif
