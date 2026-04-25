#include "prep.inc"

#define _READ                _PASTE(read_,_NAME)
#define _READ_NODE           _PASTE(_READ,_node)
#define _READ_POSITIVE       _PASTE(read_positive_,_NAME)
#define _READ_NEGATIVE       _PASTE(read_negative_,_NAME)
#define _READ_NONNEGATIVE    _PASTE(read_nonnegative_,_NAME)
#define _READ_NONPOSITIVE    _PASTE(read_nonpostive_,_NAME)
