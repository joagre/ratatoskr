# standalone C libraries

# slist

Basic singly linked list

# dlist

Doubly linked list, the benefit over slist is to
be able to unlink elements without searching from
the beginnig of list. The down side is that it
require an extra pointer, prev.

# cdlist

Comparable doubly linked list gives yet an extra benefit over
dlist in that you can check if an element is before or after
an other element in the list. An order field is kept up to date during
list operations.

# avl

Avl tree keep elements in a binary balanced tree.

# avl_kv

Avl tree keep key value elements in a binary balanced tree.
Sometimes data can not be in a avl tree at the same time as
it should be in some other data structure.

# lhash

Linear hash

# lhash_kv

Linear hash with key value elements


# atom

Storage of "constant" strings

# dynarr

Dynamic array

# segarr

Segmented array

# pred

# pred_format

# pred_parse

# bignum

# crc32
