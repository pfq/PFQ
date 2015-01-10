#ifndef __KCOMPAT__
#define __KCOMPAT__

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#define min(X,Y) ((X) < (Y) ? (X) : (Y))
#define max(X,Y) ((X) > (Y) ? (X) : (Y))

typedef int bool;

static const bool false = 0;
static const bool true  = 1;

#endif /* __KCOMPAT__ */
