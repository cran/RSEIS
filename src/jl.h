

/*   #define SQR(a) ((sqrarg=(a)) == 0.0 ? 0.0 : sqrarg*sqrarg) */
#define SQR(a) (a*a)

#define MAX(a,b) ((a) >= (b) ? (a) : (b))
#define ABS(a) ((a) < (0) ? -(a) : (a))
#define PI 3.14159265358979
#define SWAP(a,b) tempr=(a);(a)=(b);(b)=tempr

#define NR_END 1
#define FREE_ARG char*

#include "jlproto.h"
