#ifndef _PIKI_H_DEFINED
#define _PIKI_H_DEFINED

/* Some generally useful functions */
#ifndef MIN
#define MIN(a,b) ((a) <= (b) ? (a) : (b))
#endif
#ifndef MAX
#define MAX(a,b) ((a) >= (b) ? (a) : (b))
#endif
#ifndef ABS
#define ABS(x) ((x) >= (0) ? (x) : -(x))
#endif
#ifndef TRUE
#define TRUE (1)
#endif
#ifndef FALSE
#define FALSE (0)
#endif


#define    FWIN 75 
#define    BWIN 75 
#define    PIKTHR 3.0 
#define    OFFTHR 2.0 
#define    RNG 150 
#define    GTCOR_EXP -.36 
#define    GTERR_EXP -.75 
#define    PICKCORR FALSE 
#define    SEIS_SCALE 10.



int jpiki ( int *, int *, int *, int [], int, int );
int fbrat(int *, float *, int, int, int, int);


#endif /* _PIKI_H_DEFINED */
