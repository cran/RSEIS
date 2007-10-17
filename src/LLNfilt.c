#include <malloc.h>
/*
   Package for general recursion filtering; taken from SAC recursion
   filtering package written by Dave Harris (Copyright, Lawrence
   Livermore National Laboratory);  Converted to C then cleaned
   up with some modification by Bob Crosson, 5/91.  Copyright notice
   from original code follows:
   
	Copyright 1990  Regents of the University of California
	Author:  Dave Harris
		Lawrence Livermore National Laboratory
		L-205
		P.O. Box 808
		Livermore, CA  94550
		USA
		(415) 423-0617

  GENERAL:  There are two external routines in this package that are
  available to the user: "design", and "apply".  The first
  function builds the appropriate filter coefficients according to
  the users specifications, and stores them in static variables within
  the package.  The second function does the actual recursive filtering
  using the specified filter.  Whenever a filter must be changed, a
  new call to the "design" function must be made; otherwise repeated
  filtering of new data may be carried out efficiently with the current
  filter by calling "apply" whenever a new series is to be filtered.
  See the Livermore document for the filter package: XAPIIR for more
  details on the design and use of these filters (Fortran version).

  USAGE:

  design(iord, type, aproto, a, trbndw, fl, fh, ts)

	int iord:      number of poles for filter (< 10; preferably < 5)
	char *type:    "LP" for low pass (SAC default = LP)
	               "HP" for high pass
	               "BP" for band pass
	               "BR" for band reject
	char *proto:   "BU" for Butterworth (SAC default = BU)
	               "BE" for Bessel
	               "C1" for Chebyshev type 1
       	               "C2" for Chebyshev type 2
	float a:       Chebyshev stop band attenuation (ignored for others)
	               (SAC uses 30.0 for the default)
	float trbndw:  Chebyshev transition bandwidth (ignored for others)
	               (SAC uses 0.3 for the default)
	float fl:      high pass corner freq. (ignored if type LP)
	               (SAC default = 0.5)
	float fh:      low pass corner freq. (ignored if type HP)
	               (SAC default = 5.0)
	float ts:      time sample interval in secs (e.g.,0.01 = 100 samp/sec)
	               (SAC default = 0.01)

   apply(data, ndata, zp)

	float data[]:  input data array
	int ndata:     length of data
	int zp:        = 1 (TRUE) for zero phase filtering;
	               = 0 (FALSE) for normal forward filtering
*/

#include <string.h>
#include <stdio.h>
#include <math.h>
#define MIN(a,b) ((a) <= (b) ? (a) : (b))
#define MAX(a,b) ((a) >= (b) ? (a) : (b))
#define ABS(a) ((a) < (0) ? -(a) : (a))

#define TRUE 1
#define FALSE 0

typedef struct {float r, i;} complex;

/* Table of constant values */
static float c_b12 = (float)2.;
static complex c_b43 = {(float)1.,(float)0.};
static float sn[50],sd[50];
static int nsects;

/* Local function prototypes */
static double warp();
static int chebparm(), lptbp(), lptbr(), lpthp(), bilin2(), lp();
static int c1roots(), c2roots();
static int cutoffs(), beroots(), buroots();
static complex cmul(), cpowi(), jcsqrt(), conjg(), cdiv();

/*  Subroutine to apply an iir filter to a data sequence. */
/*    The filter is assumed to be stored as second order sections. */
/*    Filtering is in-place. */
/*    Zero-phase (forward and reverse) is an option. */
/*  Input Arguments: */
/*  ---------------- */
/*    DATA                           Array containing data */
/*    NSAMPS                         Number of data samples */
/*    ZP                             Logical variable, true for */
/*                                     zero phase filtering, false */
/*                                     for single pass filtering */
/*  Output Arguments: */
/*  ----------------- */
/*    DATA                          Data array (same as input) */

/* FUNC DEF */ void apply(float *data, int  nsamps, int zp)

{
    /* System generated locals */
    int i__1, i__2;

    /* Local variables */
    int jptr, i, j;
    float b0, b1, b2, a1, a2, x1, x2, y1, y2, output;

    /* Parameter adjustments */
    --data;

    /* Function Body */
    jptr = 1;
    i__1 = nsects;
    for (j = 1; j <= i__1; ++j) {
	x1 = (float)0.;
	x2 = (float)0.;
	y1 = (float)0.;
	y2 = (float)0.;
	b0 = sn[jptr];
	b1 = sn[jptr + 1];
	b2 = sn[jptr + 2];
	a1 = sd[jptr + 1];
	a2 = sd[jptr + 2];
	i__2 = nsamps;




  
#if 0
fprintf(stderr,"APPLY: %f %f %f %f \n", b0, b1, b2, a1, a2);
            fprintf(stderr, "number of samples: %d\n", nsamps);
          for(j=379; j<= 379+25; j++)
            fprintf(stderr, "%d %f\n", j, data[j]);
#endif

	for (i = 1; i <= i__2; ++i) {
	    output = b0 * data[i] + b1 * x1 + b2 * x2;
	    output -= a1 * y1 + a2 * y2;
	    y2 = y1;
	    y1 = output;
	    x2 = x1;
	    x1 = data[i];
	    data[i] = output;
	}
	jptr += 3;
    }
    if (zp) {
	jptr = 1;
	i__1 = nsects;
	for (j = 1; j <= i__1; ++j) {
	    x1 = (float)0.;
	    x2 = (float)0.;
	    y1 = (float)0.;
	    y2 = (float)0.;
	    b0 = sn[jptr];
	    b1 = sn[jptr + 1];
	    b2 = sn[jptr + 2];
	    a1 = sd[jptr + 1];
	    a2 = sd[jptr + 2];
	    for (i = nsamps; i >= 1; --i) {
		output = b0 * data[i] + b1 * x1 + b2 * x2;
		output -= a1 * y1 + a2 * y2;
		y2 = y1;
		y1 = output;
		x2 = x1;
		x1 = data[i];
		data[i] = output;
	    }
	    jptr += 3;
	}
    }

#if 0
            fprintf(stderr, "number of samples: %d\n", nsamps);
          for(j=379; j<= 379+25; j++)
            fprintf(stderr, "%d %f\n", j, data[j]);
#endif

    return;
}

/*  Subroutine to design IIR digital filters from analog prototypes. */
/*  Input Arguments: */
/*  ---------------- */
/*    IORD                Filter order (10 MAXIMUM) */
/*    TYPE                Character*2 variable containing filter type */
/*                          LOWPASS (LP) */
/*                          HIGHPASS (HP) */
/*                          BANDPASS (BP) */
/*                          BANDREJECT (BR) */
/*   APROTO              Character*2 variable designating analog prototype*/
/*                          Butterworth (BU) */
/*                          Bessel (BE) */
/*                          Chebyshev Type I (C1) */
/*                          Chebyshev Type II (C2) */
/*    A                   Chebyshev stopband attenuation factor */
/*    TRBNDW              Chebyshev transition bandwidth (fraction of */
/*                          lowpass prototype passband width) */
/*    FL                  Low-frequency cutoff */
/*    FH                  High-frequency cutoff */
/*    TS                  Sampling interval (in seconds) */
/*  Output Arguments: */
/*  ----------------- */
/*    SN                  Array containing numerator coefficients of */
/*                        second-order sections packed head-to-tail. */
/*    SD                  Array containing denominator coefficients */
/*                        of second-order sections packed head-to-tail. */

/* FUNC DEF */ void design(int iord, char *type, char *aproto,
        float a, float trbndw, float fl, float fh,float ts)
{
    /* System generated locals */
     /*  int i; */
    float r__1;

    /* Local variables */
    complex p[10], z[10];
    char stype[3*10];
    float omegar, ripple;
    float fhw, eps, flw, dcvalue;

#if 0
fprintf(stderr,"DESIGN: %d %s %s %f %f %f %f %f\n",iord, type, aproto, a, trbndw, fl, fh, ts);
#endif
    /*  Analog prototype selection */

    if (strncmp(aproto, "BU", 2) == 0) {
	buroots(p, stype, &dcvalue, iord);
    } else if (strncmp(aproto, "BE", 2) == 0) {
	beroots(p, stype, &dcvalue, iord);
    } else if (strncmp(aproto, "C1", 2) == 0) {
	chebparm(a, trbndw, iord, &eps, &ripple);
	c1roots(p, stype, &dcvalue, iord, &eps);
    } else if (strncmp(aproto, "C2", 2) == 0) {
	omegar = trbndw + (float)1.;
	c2roots(p, z, stype, &dcvalue, iord, a, omegar);
    }

    /*  Analog mapping selection */

    if (strncmp(type, "BP", 2) == 0) {
	r__1 = fl * ts / (float)2.;
	flw = warp(&r__1, &c_b12);
	r__1 = fh * ts / (float)2.;
	fhw = warp(&r__1, &c_b12);
	lptbp(p, z, stype, &dcvalue, &flw, &fhw);
    } else if (strncmp(type, "BR", 2) == 0) {
	r__1 = fl * ts / (float)2.;
	flw = warp(&r__1, &c_b12);
	r__1 = fh * ts / (float)2.;
	fhw = warp(&r__1, &c_b12);
	lptbr(p, z, stype, &dcvalue, &flw, &fhw, &sn[1], &sd[1]);
    } else if (strncmp(type, "LP", 2) == 0) {
	r__1 = fh * ts / (float)2.;
	fhw = warp(&r__1, &c_b12);
	lp(p, z, stype, &dcvalue, &sn[1], &sd[1]);
	cutoffs(&sn[1], &sd[1], &fhw);
    } else if (strncmp(type, "HP", 2) == 0) {
	r__1 = fl * ts / (float)2.;
	flw = warp(&r__1, &c_b12);
	lpthp(p, z, stype, &dcvalue, &sn[1], &sd[1]);
	cutoffs(&sn[1], &sd[1], &flw);
    }

    /*  Bilinear analog to digital transformation */
    bilin2(&sn[1], &sd[1]);

#if 0
fprintf(stderr,"nsects = %d\n",nsects);
for(i=0; i<= nsects; i++)
        fprintf(stderr,"SN SD: %f %f \n", sn[i], sd[i] );
#endif

    return;
}

/* BUROOTS -- SUBROUTINE TO COMPUTE BUTTERWORTH POLES FOR */
/*   NORMALIZED LOWPASS FILTER */
/* LAST MODIFIED:  SEPTEMBER 7, 1990 */
/*  OUTPUT ARGUMENTS: */
/*  ----------------- */
/*      P              COMPLEX ARRAY CONTAINING POLES */
/*                       CONTAINS ONLY ONE FROM EACH */
/*                       COMPLEX CONJUGATE PAIR, AND */
/*                       ALL REAL POLES */
/*      RTYPE          CHARACTER ARRAY INDICATING 2ND ORDER SECTION */
/*                       TYPE: */
/*                         (SP)  SINGLE REAL POLE */
/*                         (CP)  COMPLEX CONJUGATE POLE PAIR */
/*                         (CPZ) COMPLEX CONJUGATE POLE-ZERO PAIRS */
/*      DCVALUE        MAGNITUDE OF FILTER AT ZERO FREQUENCY */
/*  INPUT ARGUMENTS: */
/*  ---------------- */
/*      IORD           DESIRED FILTER ORDER */

/* FUNC DEF */ static int buroots(complex *p, char *rtype, float *dcvalue, int iord)
{
    /* System generated locals */
    int i__1, i__2;
    double d__1, d__2;
    complex q__1;

    /* Local variables */
    int half, k;
    float angle, pi;

    /* Parameter adjustments */
    rtype -= 3;
    --p;

    /* Function Body */
    pi = (float)3.14159265;

    half = iord / 2;

    /* TEST FOR ODD ORDER, AND ADD POLE AT -1 */

    nsects = 0;
    if (half << 1 < iord) {
	p[1].r = (float)-1., p[1].i = (float)0.;
	strncpy(rtype + 3, "SP", 2);
	nsects = 1;
    }
    i__1 = half;
    for (k = 1; k <= i__1; ++k) {
	angle = pi * ((float) ((k << 1) - 1)/(float) (iord << 1)+(float).5);
	++(nsects);
	i__2 = nsects;
	d__1 = cos(angle);
	d__2 = sin(angle);
	q__1.r = d__1, q__1.i = d__2;
	p[i__2].r = q__1.r, p[i__2].i = q__1.i;
        strncpy(rtype + nsects * 3, "CP", 2);
    }
    *dcvalue = (float)1.;
    return TRUE;
}

/*  CHEBPARM - Calculates Chebyshev type I and II design parameters */
/*  INPUT ARGUMENTS */
/*  --------------- */
/*       A                Desired stopband attenuation */
/*                          i.e. max stopband amplitude is 1/ATTEN */
/*       TRBNDW           Transition bandwidth between stop and passbands */
/*                          as a fraction of the passband width */
/*       IORD             Filter order (number of poles) */
/*  OUTPUT ARGUMENTS */
/*  ---------------- */
/*       EPS              Chebyshev passband parameter */
/*       RIPPLE           Passband ripple */

/* FUNC DEF */ static int chebparm(a, trbndw, iord, eps, ripple)
float a, trbndw;
int iord;
float *eps, *ripple;
{
    /* System generated locals */
    float r__1, r__2;

    /* Local variables */
    float g, alpha, omegar;

    omegar = trbndw + 1.;
    /* Computing 2nd power */
    r__2 = omegar;
    r__1 = omegar + sqrt(r__2 * r__2 - 1.);
    alpha = pow((double)r__1, (double)iord);
    /* Computing 2nd power */
    r__1 = alpha;
    g = (r__1 * r__1 + (float)1.) / (alpha * (float)2.);
    /* Computing 2nd power */
    r__1 = a;
    *eps = sqrt(r__1 * r__1 - (float)1.) / g;
    /* Computing 2nd power */
    r__1 = *eps;
    *ripple = (float)1. / sqrt(r__1 * r__1 + (float)1.);

    return TRUE;
}

/* C1ROOTS -- SUBROUTINE TO COMPUTE CHEBYSHEV TYPE I POLES FOR */
/*   NORMALIZED LOWPASS FILTER */
/* LAST MODIFIED:  SEPTEMBER 7, 1990 */
/*  OUTPUT ARGUMENTS: */
/*  ----------------- */
/*      P              COMPLEX ARRAY CONTAINING POLES */
/*                       CONTAINS ONLY ONE FROM EACH */
/*                       COMPLEX CONJUGATE PAIR, AND */
/*                       ALL REAL POLES */
/*      RTYPE          CHARACTER ARRAY INDICATING 2ND ORDER SECTION */
/*                       TYPE: */
/*                         (SP)  SINGLE REAL POLE */
/*                         (CP)  COMPLEX CONJUGATE POLE PAIR */
/*                         (CPZ) COMPLEX CONJUGATE POLE-ZERO PAIRS */
/*      DCVALUE        RESPONSE OF FILTER AT ZERO FREQUENCY */
/*  INPUT ARGUMENTS: */
/*  ---------------- */
/*      IORD           DESIRED FILTER ORDER */
/*      EPS            CHEBYSHEV PARAMETER RELATED TO PASSBAND RIPPLE */

/* FUNC DEF */ static int c1roots(p, rtype, dcvalue, iord, eps)
complex *p;
char *rtype;
float *dcvalue;
int iord;
float *eps;
{
    /* System generated locals */
    int i__1, i__2;
    float r__1;
    double d__1;
    complex q__1;

    /* Local variables */
    int half;
    float c;
    int i;
    float gamma, s, angle, omega, sigma, pi;

    /* Parameter adjustments */
    rtype -= 3;
    --p;

    /* Function Body */
    pi = (float)3.14159265;
    half = iord / 2;

    /*  INTERMEDIATE DESIGN PARAMETERS */

    gamma = (sqrt(*eps * *eps + (float)1.) + (float)1.) / *eps;
    gamma = log(gamma) / (float) (iord);
    gamma = exp(gamma);
    s = (gamma - (float)1. / gamma) * (float).5;
    c = (gamma + (float)1. / gamma) * (float).5;

    /*  CALCULATE POLES */

    nsects = 0;
    i__1 = half;
    for (i = 1; i <= i__1; ++i) {
	strncpy(rtype + i * 3, "CP", 2);
	angle = (float) ((i << 1) - 1) * pi / (float) (iord << 1);
	sigma = -(double)s * sin(angle);
	omega = c * cos(angle);
	i__2 = i;
	q__1.r = sigma, q__1.i = omega;
	p[i__2].r = q__1.r, p[i__2].i = q__1.i;
	++(nsects);
    }
    if (half << 1 < iord) {
	strncpy(rtype + (half + 1) * 3, "SP", 2);
	i__1 = half + 1;
	d__1 = -(double)s;
	q__1.r = d__1, q__1.i = (float)0.;
	p[i__1].r = q__1.r, p[i__1].i = q__1.i;
	++(nsects);
	*dcvalue = (float)1.;
    } else {
    /* Computing 2nd power */
	r__1 = *eps;
	*dcvalue = (float)1. / sqrt(r__1 * r__1 + 1);
    }
    return TRUE;
}

/* C2ROOTS -- SUBROUTINE TO COMPUTE ROOTS FOR NORMALIZED LOWPASS */
/*   CHEBYSHEV TYPE 2 FILTER */
/* LAST MODIFIED:  SEPTEMBER 7, 1990 */
/*  OUTPUT ARGUMENTS: */
/*  ----------------- */
/*      P              COMPLEX ARRAY CONTAINING POLES */
/*                       CONTAINS ONLY ONE FROM EACH */
/*                       COMPLEX CONJUGATE PAIR, AND */
/*                       ALL REAL POLES */
/*      Z              COMPLEX ARRAY CONTAINING ZEROS */
/*                       CONTAINS ONLY ONE FROM EACH */
/*                       COMPLEX CONJUGATE PAIR, AND */
/*                       ALL REAL ZEROS */
/*      RTYPE          CHARACTER ARRAY INDICATING 2ND ORDER SECTION */
/*                       TYPE: */
/*                         (SP)  SINGLE REAL POLE */
/*                         (CP)  COMPLEX CONJUGATE POLE PAIR */
/*                         (CPZ) COMPLEX CONJUGATE POLE-ZERO PAIRS */
/*      DCVALUE        MAGNITUDE OF FILTER AT ZERO FREQUENCY */
/*  INPUT ARGUMENTS: */
/*  ---------------- */
/*      IORD           DESIRED FILTER ORDER */
/*      A              STOPBAND ATTENUATION FACTOR */
/*      OMEGAR         CUTOFF FREQUENCY OF STOPBAND */
/*                     PASSBAND CUTOFF IS AT 1.0 HERTZ */

/* FUNC DEF */ static int c2roots(p, z, rtype, dcvalue, iord, a, omegar)
complex *p, *z;
char *rtype;
float *dcvalue;
int iord;
float a, omegar;
{
    /* System generated locals */
    int i__1, i__2;
    double d__1;
    complex q__1;

    /* Local variables */
    int half;
    float beta, c;
    int i;
    float gamma, s, alpha, angle, omega, sigma, denom, pi;

    /* Parameter adjustments */
    rtype -= 3;
    --z;
    --p;

    /* Function Body */
    pi = (float)3.14159265;
    half = iord / 2;

/*  INTERMEDIATE DESIGN PARAMETERS */

    gamma = a + sqrt(a * a - (float)1.);
    gamma = log(gamma) / (float) (iord);
    gamma = exp(gamma);
    s = (gamma - (float)1. / gamma) * (float).5;
    c = (gamma + (float)1. / gamma) * (float).5;
    nsects = 0;
    i__1 = half;
    for (i = 1; i <= i__1; ++i) {

/*  CALCULATE POLES */

	strncpy(rtype + i * 3, "CPZ", 3);
	angle = (float) ((i << 1) - 1) * pi / (float) (iord << 1);
	alpha = -(double)s * sin(angle);
	beta = c * cos(angle);
	denom = alpha * alpha + beta * beta;
	sigma = omegar * alpha / denom;
	omega = -(double)(omegar) * beta / denom;
	i__2 = i;
	q__1.r = sigma, q__1.i = omega;
	p[i__2].r = q__1.r, p[i__2].i = q__1.i;

/*  CALCULATE ZEROS */

	omega = omegar / cos(angle);
	i__2 = i;
	q__1.r = (float)0., q__1.i = omega;
	z[i__2].r = q__1.r, z[i__2].i = q__1.i;
	++(nsects);
    }

/*  ODD-ORDER FILTERS */

    if (half << 1 < iord) {
	strncpy(rtype + (half + 1) * 3, "SP", 2);
	i__1 = half + 1;
	d__1 = -(double)(omegar) / s;
	q__1.r = d__1, q__1.i = (float)0.;
	p[i__1].r = q__1.r, p[i__1].i = q__1.i;
	++(nsects);
    }

/*  DC VALUE */

    *dcvalue = 1.;
    return TRUE;
}

/* Subroutine to convert an prototype lowpass filter to a bandpass filter via
*/
/*    the analog polynomial transformation.  The lowpass filter is */
/*   described in terms of its poles and zeros (as input to this routine).*/
/*    The output consists of the parameters for second order sections. */
/*  Input Arguments: */
/*  ---------------- */
/*    P                       Array containing poles */
/*    Z                       Array containing zeros */
/*    RTYPE                   Character array containing type information */
/*                              (SP) single real pole  or */
/*                              (CP) complex conjugate pole pair  or */
/*                              (CPZ) complex conjugate pole/zero pairs */
/*    DCVALUE                 Zero frequency value of filter */
/*    FL                      Low-frequency cutoff */
/*    FH                      High-frequency cutoff */
/*  Output Arguments: */
/*  ----------------- */
/*    SN                      Numerator polynomials for second order */
/*                              sections. */
/*    SD                      Denominator polynomials for second order */
/*                              sections. */
/*                              This subroutine doubles the number of */
/*                              sections. */

/* FUNC DEF */ static int lptbp(p, z, rtype, dcvalue, fl, fh)
complex *p, *z;
char *rtype;
float *dcvalue;
float *fl, *fh;
{
    /* System generated locals */
    int i__1, i__2, i__3, i__4, i__5, i__6, i__7;
    double d__1;
    complex q__1, q__2, q__3, q__4, q__5, q__6, q__7, q__8, q__9, q__10;

    /* Local variables */
    int iptr;
    float a, b;
    complex h;
    int i, n;
    complex s;
    float scale;
    complex ctemp, p1, p2;
    float twopi;
    complex z1, z2;
    float pi;

    /* Parameter adjustments */
    rtype -= 3;
    --z;
    --p;

    /* Function Body */
    pi = (float)3.14159265;
    twopi = pi * (float)2.;
    a = twopi * twopi * *fl * *fh;
    b = twopi * (*fh - *fl);

    n = nsects;
    nsects = 0;
    iptr = 1;
    i__1 = n;
    for (i = 1; i <= i__1; ++i) {
	if (strncmp(rtype + i * 3, "CPZ", 3) == 0) {
	    i__2 = i;
	    q__3.r = b * z[i__2].r, q__3.i = b * z[i__2].i;
            q__2 = cpowi(q__3, 2);
	    d__1 = a * (float)4.;
	    q__1.r = q__2.r - d__1, q__1.i = q__2.i;
	    ctemp.r = q__1.r, ctemp.i = q__1.i;
            q__1 = jcsqrt(ctemp);
	    ctemp.r = q__1.r, ctemp.i = q__1.i;
	    i__2 = i;
	    q__3.r = b * z[i__2].r, q__3.i = b * z[i__2].i;
	    q__2.r = q__3.r + ctemp.r, q__2.i = q__3.i + ctemp.i;
	    q__1.r = q__2.r * (float).5, q__1.i = q__2.i * (float).5;
	    z1.r = q__1.r, z1.i = q__1.i;
	    i__2 = i;
	    q__3.r = b * z[i__2].r, q__3.i = b * z[i__2].i;
	    q__2.r = q__3.r - ctemp.r, q__2.i = q__3.i - ctemp.i;
	    q__1.r = q__2.r * (float).5, q__1.i = q__2.i * (float).5;
	    z2.r = q__1.r, z2.i = q__1.i;
	    i__2 = i;
	    q__3.r = b * p[i__2].r, q__3.i = b * p[i__2].i;
            q__2 = cpowi(q__3, 2);
	    d__1 = a * (float)4.;
	    q__1.r = q__2.r - d__1, q__1.i = q__2.i;
	    ctemp.r = q__1.r, ctemp.i = q__1.i;
            q__1 = jcsqrt(ctemp);
	    ctemp.r = q__1.r, ctemp.i = q__1.i;
	    i__2 = i;
	    q__3.r = b * p[i__2].r, q__3.i = b * p[i__2].i;
	    q__2.r = q__3.r + ctemp.r, q__2.i = q__3.i + ctemp.i;
	    q__1.r = q__2.r * (float).5, q__1.i = q__2.i * (float).5;
	    p1.r = q__1.r, p1.i = q__1.i;
	    i__2 = i;
	    q__3.r = b * p[i__2].r, q__3.i = b * p[i__2].i;
	    q__2.r = q__3.r - ctemp.r, q__2.i = q__3.i - ctemp.i;
	    q__1.r = q__2.r * (float).5, q__1.i = q__2.i * (float).5;
	    p2.r = q__1.r, p2.i = q__1.i;
            q__2 = conjg(z1);
	    q__1.r = z1.r * q__2.r - z1.i * q__2.i, q__1.i = z1.r * q__2.i + 
		    z1.i * q__2.r;
	    sn[iptr] = q__1.r;
	    sn[iptr + 1] = z1.r * (float)-2.;
	    sn[iptr + 2] = (float)1.;
            q__2 = conjg(p1);
	    q__1.r = p1.r * q__2.r - p1.i * q__2.i, q__1.i = p1.r * q__2.i + 
		    p1.i * q__2.r;
	    sd[iptr] = q__1.r;
	    sd[iptr + 1] = p1.r * (float)-2.;
	    sd[iptr + 2] = (float)1.;
	    iptr += 3;
            q__2 = conjg(z2);
	    q__1.r = z2.r * q__2.r - z2.i * q__2.i, q__1.i = z2.r * q__2.i + 
		    z2.i * q__2.r;
	    sn[iptr] = q__1.r;
	    sn[iptr + 1] = z2.r * (float)-2.;
	    sn[iptr + 2] = (float)1.;
            q__2 = conjg(p2);
	    q__1.r = p2.r * q__2.r - p2.i * q__2.i, q__1.i = p2.r * q__2.i + 
		    p2.i * q__2.r;
	    sd[iptr] = q__1.r;
	    sd[iptr + 1] = p2.r * (float)-2.;
	    sd[iptr + 2] = (float)1.;
	    iptr += 3;
	    nsects += 2;
	} else if (strncmp(rtype + i * 3, "CP", 2) == 0) {
	    i__2 = i;
	    q__3.r = b * p[i__2].r, q__3.i = b * p[i__2].i;
            q__2 = cpowi(q__3, 2);
	    d__1 = a * (float)4.;
	    q__1.r = q__2.r - d__1, q__1.i = q__2.i;
	    ctemp.r = q__1.r, ctemp.i = q__1.i;
            q__1 = jcsqrt(ctemp);
	    ctemp.r = q__1.r, ctemp.i = q__1.i;
	    i__2 = i;
	    q__3.r = b * p[i__2].r, q__3.i = b * p[i__2].i;
	    q__2.r = q__3.r + ctemp.r, q__2.i = q__3.i + ctemp.i;
	    q__1.r = q__2.r * (float).5, q__1.i = q__2.i * (float).5;
	    p1.r = q__1.r, p1.i = q__1.i;
	    i__2 = i;
	    q__3.r = b * p[i__2].r, q__3.i = b * p[i__2].i;
	    q__2.r = q__3.r - ctemp.r, q__2.i = q__3.i - ctemp.i;
	    q__1.r = q__2.r * (float).5, q__1.i = q__2.i * (float).5;
	    p2.r = q__1.r, p2.i = q__1.i;
	    sn[iptr] = (float)0.;
	    sn[iptr + 1] = b;
	    sn[iptr + 2] = (float)0.;
            q__2 = conjg(p1);
	    q__1.r = p1.r * q__2.r - p1.i * q__2.i, q__1.i = p1.r * q__2.i + 
		    p1.i * q__2.r;
	    sd[iptr] = q__1.r;
	    sd[iptr + 1] = p1.r * (float)-2.;
	    sd[iptr + 2] = (float)1.;
	    iptr += 3;
	    sn[iptr] = (float)0.;
	    sn[iptr + 1] = b;
	    sn[iptr + 2] = (float)0.;
            q__2 = conjg(p2);
	    q__1.r = p2.r * q__2.r - p2.i * q__2.i, q__1.i = p2.r * q__2.i + 
		    p2.i * q__2.r;
	    sd[iptr] = q__1.r;
	    sd[iptr + 1] = p2.r * (float)-2.;
	    sd[iptr + 2] = (float)1.;
	    iptr += 3;
	    nsects += 2;
	} else if (strncmp(rtype + i * 3, "SP", 2) == 0) {
	    sn[iptr] = (float)0.;
	    sn[iptr + 1] = b;
	    sn[iptr + 2] = (float)0.;
	    sd[iptr] = a;
	    i__2 = i;
	    sd[iptr + 1] = -(double)b * p[i__2].r;
	    sd[iptr + 2] = (float)1.;
	    iptr += 3;
	    ++(nsects);
	}
    }
/* Scaling - use fact that the bandpass filter amplitude at sqrt( omega_l **/
/*            equals the amplitude of the lowpass prototype at d.c. */
    d__1 = sqrt(a);
    q__1.r = (float)0., q__1.i = d__1;
    s.r = q__1.r, s.i = q__1.i;
    h.r = (float)1., h.i = (float)0.;

    iptr = 1;
    i__1 = nsects;
    for (i = 1; i <= i__1; ++i) {
	i__2 = iptr + 2;
	q__6.r = sn[i__2] * s.r, q__6.i = sn[i__2] * s.i;
	i__3 = iptr + 1;
	q__5.r = q__6.r + sn[i__3], q__5.i = q__6.i;
	q__4.r = q__5.r * s.r - q__5.i * s.i, q__4.i = q__5.r * s.i + q__5.i *
		 s.r;
	i__4 = iptr;
	q__3.r = q__4.r + sn[i__4], q__3.i = q__4.i;
	q__2.r = h.r * q__3.r - h.i * q__3.i, q__2.i = h.r * q__3.i + h.i * 
		q__3.r;
	i__5 = iptr + 2;
	q__10.r = sd[i__5] * s.r, q__10.i = sd[i__5] * s.i;
	i__6 = iptr + 1;
	q__9.r = q__10.r + sd[i__6], q__9.i = q__10.i;
	q__8.r = q__9.r * s.r - q__9.i * s.i, q__8.i = q__9.r * s.i + q__9.i *
		 s.r;
	i__7 = iptr;
	q__7.r = q__8.r + sd[i__7], q__7.i = q__8.i;
        q__1 = cdiv(q__2, q__7);
	h.r = q__1.r, h.i = q__1.i;
	iptr += 3;
    }
    q__2.r = *dcvalue, q__2.i = (float)0.;
    d__1 = h.r;
    q__5 = conjg(h);
    q__4.r = d__1 * q__5.r, q__4.i = d__1 * q__5.i;
    q__3 = jcsqrt(q__4);
    q__1 = cdiv(q__2, q__3);
    scale = q__1.r;
    sn[1] *= scale;
    sn[2] *= scale;
    sn[3] *= scale;
    return TRUE;
}

/*  Subroutine to generate second order section parameterization */
/*    from an pole-zero description for lowpass filters. */
/*  Input Arguments: */
/*  ---------------- */
/*    P                       Array containing poles */
/*    Z                       Array containing zeros */
/*   RTYPE                   Character array containing root type information
*/
/*                              (SP)  Single pole or */
/*                              (CP)  Complex conjugate pole pair */
/*                             (CPZ) Complex conjugate pole and zero pairs*/
/*    DCVALUE                 Zero-frequency value of prototype filter */
/*  Output Arguments: */
/*  ----------------- */
/*    SN                      Numerator polynomials for second order */
/*                              sections. */
/*    SD                      Denominator polynomials for second order */
/*                              sections. */

/* FUNC DEF */ static int lp(p, z, rtype, dcvalue, sn, sd)
complex *p, *z;
char *rtype;
float *dcvalue;
float *sn, *sd;
{
    /* System generated locals */
    int i__1, i__2, i__3;
    complex q__1, q__2, q__3, q__4;

    /* Local variables */
    int iptr, i;
    float scale;

    /* Parameter adjustments */
    --sd;
    --sn;
    rtype -= 3;
    --z;
    --p;

    /* Function Body */
    iptr = 1;
    i__1 = nsects;
    for (i = 1; i <= i__1; ++i) {
	if (strncmp(rtype + i * 3, "CPZ", 3) == 0) {
	    i__2 = i;
	    q__2 = conjg(p[i]);
	    q__1.r = p[i__2].r * q__2.r - p[i__2].i * q__2.i, q__1.i = p[i__2]
		    .r * q__2.i + p[i__2].i * q__2.r;
	    i__3 = i;
	    q__4 = conjg(z[i]);
	    q__3.r = z[i__3].r * q__4.r - z[i__3].i * q__4.i, q__3.i = z[i__3]
		    .r * q__4.i + z[i__3].i * q__4.r;
	    scale = q__1.r / q__3.r;
	    i__2 = i;
	    q__2 = conjg(z[i]);
	    q__1.r = z[i__2].r * q__2.r - z[i__2].i * q__2.i, q__1.i = z[i__2]
		    .r * q__2.i + z[i__2].i * q__2.r;
	    sn[iptr] = q__1.r * scale;
	    i__2 = i;
	    sn[iptr + 1] = z[i__2].r * (float)-2. * scale;
	    sn[iptr + 2] = scale * (float)1.;
	    i__2 = i;
	    q__2 = conjg(p[i]);
	    q__1.r = p[i__2].r * q__2.r - p[i__2].i * q__2.i, q__1.i = p[i__2]
		    .r * q__2.i + p[i__2].i * q__2.r;
	    sd[iptr] = q__1.r;
	    i__2 = i;
	    sd[iptr + 1] = p[i__2].r * (float)-2.;
	    sd[iptr + 2] = (float)1.;
	    iptr += 3;
	} else if (strncmp(rtype + i * 3, "CP", 2) == 0) {
	    i__2 = i;
	    q__2 = conjg(p[i]);
	    q__1.r = p[i__2].r * q__2.r - p[i__2].i * q__2.i, q__1.i = p[i__2]
		    .r * q__2.i + p[i__2].i * q__2.r;
	    scale = q__1.r;
	    sn[iptr] = scale;
	    sn[iptr + 1] = (float)0.;
	    sn[iptr + 2] = (float)0.;
	    i__2 = i;
	    q__2 = conjg(p[i]);
	    q__1.r = p[i__2].r * q__2.r - p[i__2].i * q__2.i, q__1.i = p[i__2]
		    .r * q__2.i + p[i__2].i * q__2.r;
	    sd[iptr] = q__1.r;
	    i__2 = i;
	    sd[iptr + 1] = p[i__2].r * (float)-2.;
	    sd[iptr + 2] = (float)1.;
	    iptr += 3;
	} else if (strncmp(rtype + i * 3, "SP", 2) == 0) {
	    i__2 = i;
	    scale = -(double)p[i__2].r;
	    sn[iptr] = scale;
	    sn[iptr + 1] = (float)0.;
	    sn[iptr + 2] = (float)0.;
	    i__2 = i;
	    sd[iptr] = -(double)p[i__2].r;
	    sd[iptr + 1] = (float)1.;
	    sd[iptr + 2] = (float)0.;
	    iptr += 3;
	}
    }
    sn[1] = *dcvalue * sn[1];
    sn[2] = *dcvalue * sn[2];
    sn[3] = *dcvalue * sn[3];
    return TRUE;
}

/*  Subroutine to convert a lowpass filter to a band reject filter */
/*    via an analog polynomial transformation.  The lowpass filter is */
/*   described in terms of its poles and zeros (as input to this routine).*/
/*    The output consists of the parameters for second order sections. */
/*  Input Arguments: */
/*  ---------------- */
/*    P                       Array containing poles */
/*    Z                       Array containing zeros */
/*    RTYPE                   Character array containing type information */
/*                              (SP)  single real pole or */
/*                              (CP)  complex conjugate pole pair */
/*                              (CPZ) complex conjugate pole/zero pairs */
/*    DCVALUE                 Zero-frequency value of prototype filter */
/*                              prior to transformation */
/*    FL                      Low-frequency cutoff */
/*    FH                      High-frequency cutoff */
/*  Output Arguments: */
/*  ----------------- */
/*    SN                      Numerator polynomials for second order */
/*                              sections. */
/*    SD                      Denominator polynomials for second order */
/*                              sections. */
/*    NSECTS                  Number of second order sections following */
/*                              transformation.  The number is doubled. */

/* FUNC DEF */ static int lptbr(p, z, rtype, dcvalue, fl, fh, sn, sd)
complex *p, *z;
char *rtype;
float *dcvalue;
float *fl, *fh, *sn, *sd;
{
    /* System generated locals */
    int i__1, i__2;
    double d__1;
    complex q__1, q__2, q__3;

    /* Local variables */
    complex cinv;
    int iptr;
    float a, b, h;
    int i, n;
    float scale;
    complex ctemp, p1, p2;
    float twopi;
    complex z1, z2;
    float pi;

    /* Parameter adjustments */
    --sd;
    --sn;
    rtype -= 3;
    --z;
    --p;

    /* Function Body */
    pi = (float)3.14159265;
    twopi = pi * (float)2.;
    a = twopi * twopi * *fl * *fh;
    b = twopi * (*fh - *fl);
    n = nsects;
    nsects = 0;
    iptr = 1;
    i__1 = n;
    for (i = 1; i <= i__1; ++i) {
	if (strncmp(rtype + i * 3, "CPZ", 3) == 0) {
	    q__1 = cdiv(c_b43, z[i]);
	    cinv.r = q__1.r, cinv.i = q__1.i;
	    q__3.r = b * cinv.r, q__3.i = b * cinv.i;
            q__2 = cpowi(q__3, 2);
	    d__1 = a * (float)4.;
	    q__1.r = q__2.r - d__1, q__1.i = q__2.i;
	    ctemp.r = q__1.r, ctemp.i = q__1.i;
	    q__1 = jcsqrt(ctemp);
	    ctemp.r = q__1.r, ctemp.i = q__1.i;
	    q__3.r = b * cinv.r, q__3.i = b * cinv.i;
	    q__2.r = q__3.r + ctemp.r, q__2.i = q__3.i + ctemp.i;
	    q__1.r = q__2.r * (float).5, q__1.i = q__2.i * (float).5;
	    z1.r = q__1.r, z1.i = q__1.i;
	    q__3.r = b * cinv.r, q__3.i = b * cinv.i;
	    q__2.r = q__3.r - ctemp.r, q__2.i = q__3.i - ctemp.i;
	    q__1.r = q__2.r * (float).5, q__1.i = q__2.i * (float).5;
	    z2.r = q__1.r, z2.i = q__1.i;
	    q__1 = cdiv(c_b43, p[i]);
	    cinv.r = q__1.r, cinv.i = q__1.i;
	    q__3.r = b * cinv.r, q__3.i = b * cinv.i;
            q__2 = cpowi(q__3, 2);
	    d__1 = a * (float)4.;
	    q__1.r = q__2.r - d__1, q__1.i = q__2.i;
	    ctemp.r = q__1.r, ctemp.i = q__1.i;
	    q__1 = jcsqrt(ctemp);
	    ctemp.r = q__1.r, ctemp.i = q__1.i;
	    q__3.r = b * cinv.r, q__3.i = b * cinv.i;
	    q__2.r = q__3.r + ctemp.r, q__2.i = q__3.i + ctemp.i;
	    q__1.r = q__2.r * (float).5, q__1.i = q__2.i * (float).5;
	    p1.r = q__1.r, p1.i = q__1.i;
	    q__3.r = b * cinv.r, q__3.i = b * cinv.i;
	    q__2.r = q__3.r - ctemp.r, q__2.i = q__3.i - ctemp.i;
	    q__1.r = q__2.r * (float).5, q__1.i = q__2.i * (float).5;
	    p2.r = q__1.r, p2.i = q__1.i;
	    q__2 = conjg(z1);
	    q__1.r = z1.r * q__2.r - z1.i * q__2.i, q__1.i = z1.r * q__2.i + 
		    z1.i * q__2.r;
	    sn[iptr] = q__1.r;
	    sn[iptr + 1] = z1.r * (float)-2.;
	    sn[iptr + 2] = (float)1.;
	    q__2 = conjg(p1);
	    q__1.r = p1.r * q__2.r - p1.i * q__2.i, q__1.i = p1.r * q__2.i + 
		    p1.i * q__2.r;
	    sd[iptr] = q__1.r;
	    sd[iptr + 1] = p1.r * (float)-2.;
	    sd[iptr + 2] = (float)1.;
	    iptr += 3;
	    q__2 = conjg(z2);
	    q__1.r = z2.r * q__2.r - z2.i * q__2.i, q__1.i = z2.r * q__2.i + 
		    z2.i * q__2.r;
	    sn[iptr] = q__1.r;
	    sn[iptr + 1] = z2.r * (float)-2.;
	    sn[iptr + 2] = (float)1.;
	    q__2 = conjg(p2);
	    q__1.r = p2.r * q__2.r - p2.i * q__2.i, q__1.i = p2.r * q__2.i + 
		    p2.i * q__2.r;
	    sd[iptr] = q__1.r;
	    sd[iptr + 1] = p2.r * (float)-2.;
	    sd[iptr + 2] = (float)1.;
	    iptr += 3;
	    nsects += 2;
	} else if (strncmp(rtype + i * 3, "CP", 2) == 0) {
	    q__1 = cdiv(c_b43, p[i]);
	    cinv.r = q__1.r, cinv.i = q__1.i;
	    q__3.r = b * cinv.r, q__3.i = b * cinv.i;
            q__2 = cpowi(q__3, 2);
	    d__1 = a * (float)4.;
	    q__1.r = q__2.r - d__1, q__1.i = q__2.i;
	    ctemp.r = q__1.r, ctemp.i = q__1.i;
	    q__1 = jcsqrt(ctemp);
	    ctemp.r = q__1.r, ctemp.i = q__1.i;
	    q__3.r = b * cinv.r, q__3.i = b * cinv.i;
	    q__2.r = q__3.r + ctemp.r, q__2.i = q__3.i + ctemp.i;
	    q__1.r = q__2.r * (float).5, q__1.i = q__2.i * (float).5;
	    p1.r = q__1.r, p1.i = q__1.i;
	    q__3.r = b * cinv.r, q__3.i = b * cinv.i;
	    q__2.r = q__3.r - ctemp.r, q__2.i = q__3.i - ctemp.i;
	    q__1.r = q__2.r * (float).5, q__1.i = q__2.i * (float).5;
	    p2.r = q__1.r, p2.i = q__1.i;
	    sn[iptr] = a;
	    sn[iptr + 1] = (float)0.;
	    sn[iptr + 2] = (float)1.;
	    q__2 = conjg(p1);
	    q__1.r = p1.r * q__2.r - p1.i * q__2.i, q__1.i = p1.r * q__2.i + 
		    p1.i * q__2.r;
	    sd[iptr] = q__1.r;
	    sd[iptr + 1] = p1.r * (float)-2.;
	    sd[iptr + 2] = (float)1.;
	    iptr += 3;
	    sn[iptr] = a;
	    sn[iptr + 1] = (float)0.;
	    sn[iptr + 2] = (float)1.;
	    q__2 = conjg(p2);
	    q__1.r = p2.r * q__2.r - p2.i * q__2.i, q__1.i = p2.r * q__2.i + 
		    p2.i * q__2.r;
	    sd[iptr] = q__1.r;
	    sd[iptr + 1] = p2.r * (float)-2.;
	    sd[iptr + 2] = (float)1.;
	    iptr += 3;
	    nsects += 2;
	} else if (strncmp(rtype + i * 3, "SP", 2) == 0) {
	    sn[iptr] = a;
	    sn[iptr + 1] = (float)0.;
	    sn[iptr + 2] = (float)1.;
	    i__2 = i;
	    sd[iptr] = -(double)a * p[i__2].r;
	    sd[iptr + 1] = b;
	    i__2 = i;
	    sd[iptr + 2] = -(double)p[i__2].r;
	    iptr += 3;
	    ++(nsects);
	}
    }
/*  Scaling - use the fact that the bandreject filter amplitude  at d.c. */
/*            equals the lowpass prototype amplitude at d.c. */
    h = (float)1.;
    iptr = 1;
    i__1 = nsects;
    for (i = 1; i <= i__1; ++i) {
	h = h * sn[iptr] / sd[iptr];
	iptr += 3;
    }
    scale = *dcvalue / ABS(h);
    sn[1] *= scale;
    sn[2] *= scale;
    sn[3] *= scale;
    return TRUE;
}

/*  Subroutine to alter the cutoff of a filter.  Assumes that the */
/*    filter is structured as second order sections.  Changes */
/*    the cutoffs of normalized lowpass and highpass filters through */
/*    a simple polynomial transformation. */
/*  Input Arguments: */
/*  ---------------- */
/*    F                       New cutoff frequency */
/*  Input/Output Arguments: */
/*  ----------------------- */
/*    SN                      Numerator polynomials for second order */
/*                              sections. */
/*    SD                      Denominator polynomials for second order */
/*                              sections. */

/* FUNC DEF */ static int cutoffs(sn, sd, f)
float *sn, *sd;
float *f;
{
    /* System generated locals */
    int i__1;

    /* Local variables */
    int iptr, i;
    float scale;

    /* Parameter adjustments */
    --sd;
    --sn;

    /* Function Body */
    scale = *f * (float)6.2831853000000004;

    iptr = 1;
    i__1 = nsects;
    for (i = 1; i <= i__1; ++i) {

	sn[iptr + 1] /= scale;
	sn[iptr + 2] /= scale * scale;
	sd[iptr + 1] /= scale;
	sd[iptr + 2] /= scale * scale;
	iptr += 3;
    }
    return TRUE;
}

/*  Subroutine to convert a lowpass filter to a highpass filter via */
/*    an analog polynomial transformation.  The lowpass filter is */
/*   described in terms of its poles and zeroes (as input to this routine).*/
/*    The output consists of the parameters for second order sections. */
/*  Input Arguments: */
/*  ---------------- */
/*    P                       Array containing poles */
/*    Z                       Array containing zeroes */
/*   RTYPE                   Character array containing root type information
*/
/*                              (SP) single real pole or */
/*                              (CP)  complex conjugate pair */
/*                              (CPZ) complex pole/zero pairs */
/*    DCVALUE                 Zero-frequency value of prototype filter */
/*  Output Arguments: */
/*  ----------------- */
/*    SN                      Numerator polynomials for second order */
/*                              sections. */
/*    SD                      Denominator polynomials for second order */
/*                              sections. */

/* FUNC DEF */ static int lpthp(p, z, rtype, dcvalue, sn, sd)
complex *p, *z;
char *rtype;
float *dcvalue;
float *sn, *sd;
{
    /* System generated locals */
    int i__1, i__2, i__3;
    complex q__1, q__2, q__3, q__4;

   /* Local variables */
    int iptr, i;
    float scale;

    /* Parameter adjustments */
    --sd;
    --sn;
    rtype -= 3;
    --z;
    --p;

    /* Function Body */
    iptr = 1;
    i__1 = nsects;
    for (i = 1; i <= i__1; ++i) {
	if (strncmp(rtype + i * 3, "CPZ", 3) == 0) {
	    i__2 = i;
	    q__2 = conjg(p[i]);
	    q__1.r = p[i__2].r * q__2.r - p[i__2].i * q__2.i, q__1.i = p[i__2]
		    .r * q__2.i + p[i__2].i * q__2.r;
	    i__3 = i;
	    q__4 = conjg(z[i]);
	    q__3.r = z[i__3].r * q__4.r - z[i__3].i * q__4.i, q__3.i = z[i__3]
		    .r * q__4.i + z[i__3].i * q__4.r;
	    scale = q__1.r / q__3.r;
	    sn[iptr] = scale * (float)1.;
	    i__2 = i;
	    sn[iptr + 1] = z[i__2].r * (float)-2. * scale;
	    i__2 = i;
	    q__2 = conjg(z[i]);
	    q__1.r = z[i__2].r * q__2.r - z[i__2].i * q__2.i, q__1.i = z[i__2]
		    .r * q__2.i + z[i__2].i * q__2.r;
	    sn[iptr + 2] = q__1.r * scale;
	    sd[iptr] = (float)1.;
	    i__2 = i;
	    sd[iptr + 1] = p[i__2].r * (float)-2.;
	    i__2 = i;
	    q__2 = conjg(p[i]);
	    q__1.r = p[i__2].r * q__2.r - p[i__2].i * q__2.i, q__1.i = p[i__2]
		    .r * q__2.i + p[i__2].i * q__2.r;
	    sd[iptr + 2] = q__1.r;
	    iptr += 3;
	} else if (strncmp(rtype + i * 3, "CP", 2) == 0) {
	    i__2 = i;
	    q__2 = conjg(p[i]);
	    q__1.r = p[i__2].r * q__2.r - p[i__2].i * q__2.i, q__1.i = p[i__2]
		    .r * q__2.i + p[i__2].i * q__2.r;
	    scale = q__1.r;
	    sn[iptr] = (float)0.;
	    sn[iptr + 1] = (float)0.;
	    sn[iptr + 2] = scale;
	    sd[iptr] = (float)1.;
	    i__2 = i;
	    sd[iptr + 1] = p[i__2].r * (float)-2.;
	    i__2 = i;
	    q__2 = conjg(p[i]);
	    q__1.r = p[i__2].r * q__2.r - p[i__2].i * q__2.i, q__1.i = p[i__2]
		    .r * q__2.i + p[i__2].i * q__2.r;
	    sd[iptr + 2] = q__1.r;
	    iptr += 3;
	} else if (strncmp(rtype + i * 3, "SP", 2) == 0) {
	    i__2 = i;
	    scale = -(double)p[i__2].r;
	    sn[iptr] = (float)0.;
	    sn[iptr + 1] = scale;
	    sn[iptr + 2] = (float)0.;
	    sd[iptr] = (float)1.;
	    i__2 = i;
	    sd[iptr + 1] = -(double)p[i__2].r;
	    sd[iptr + 2] = (float)0.;
	    iptr += 3;
	}
    }
    sn[1] *= *dcvalue;
    sn[2] *= *dcvalue;
    sn[3] *= *dcvalue;
    return TRUE;
}

/* Transforms an analog filter to a digital filter via the bilinear transforma
ti*/
/*   Assumes both are stored as second order sections.  The transformation is
*/
/*    done in-place. */
/*  Input Arguments: */
/*  ---------------- */
/*   SN                   Array containing numerator polynomial coefficients f
or*/
/*                           second order sections.  Packed head-to-tail. */
/*   SD                   Array containing denominator polynomial coefficients
 f*/
/*                           second order sections.  Packed head-to-tail. */

/* FUNC DEF */ static int bilin2(sn, sd)
float *sn, *sd;
{
    /* System generated locals */
    int i__1;

    /* Local variables */
    int iptr, i;
    float scale, a0, a1, a2;

    /* Parameter adjustments */
    --sd;
    --sn;

    /* Function Body */
    iptr = 1;
    i__1 = nsects;
    for (i = 1; i <= i__1; ++i) {
	a0 = sd[iptr];
	a1 = sd[iptr + 1];
	a2 = sd[iptr + 2];
	scale = a2 + a1 + a0;
	sd[iptr] = (float)1.;
	sd[iptr + 1] = (a0 - a2) * (float)2. / scale;
	sd[iptr + 2] = (a2 - a1 + a0) / scale;
	a0 = sn[iptr];
	a1 = sn[iptr + 1];
	a2 = sn[iptr + 2];
	sn[iptr] = (a2 + a1 + a0) / scale;
	sn[iptr + 1] = (a0 - a2) * (float)2. / scale;
	sn[iptr + 2] = (a2 - a1 + a0) / scale;
	iptr += 3;
    }
    return TRUE;
}

/* WARP -- FUNCTION, APPLIES TANGENT FREQUENCY WARPING TO COMPENSATE */
/*         FOR BILINEAR ANALOG -> DIGITAL TRANSFORMATION */
/* ARGUMENTS: */
/* ---------- */
/*      F       ORIGINAL DESIGN FREQUENCY SPECIFICATION (HERTZ) */
/*      TS      SAMPLING INTERVAL (SECONDS) */
/*  LAST MODIFIED:  SEPTEMBER 20, 1990 */

/* FUNC DEF */ static double warp(f, ts)
float *f, *ts;
{
    /* System generated locals */
    float ret_val;

    /* Local variables */
    float angle, twopi;

    twopi = (float)6.2831853;
    angle = twopi * *f * *ts / (float)2.;
    ret_val = tan(angle) * (float)2. / *ts;
    ret_val /= twopi;
    return ret_val;
}

/* BEROOTS -- SUBROUTINE TO RETURN BESSEL POLES FOR */
/*   NORMALIZED LOWPASS FILTER */
/* LAST MODIFIED:  SEPTEMBER 7, 1990 */
/*  OUTPUT ARGUMENTS: */
/*  ----------------- */
/*      P              COMPLEX ARRAY CONTAINING POLES */
/*                       CONTAINS ONLY ONE FROM EACH */
/*                       COMPLEX CONJUGATE PAIR, AND */
/*                       ALL REAL POLES */
/*      RTYPE          CHARACTER ARRAY INDICATING 2ND ORDER SECTION */
/*                       TYPE: */
/*                         (SP)  SINGLE REAL POLE */
/*                         (CP)  COMPLEX CONJUGATE POLE PAIR */
/*                         (CPZ) COMPLEX CONJUGATE POLE-ZERO PAIRS */
/*      DCVALUE        MAGNITUDE OF FILTER AT ZERO FREQUENCY */
/*  INPUT ARGUMENTS: */
/*  ---------------- */
/*      IORD           DESIRED FILTER ORDER */

/* FUNC DEF */ static int beroots(p, rtype, dcvalue, iord)
complex *p;
char *rtype;
float *dcvalue;
int iord;
{
    /* Parameter adjustments */
    rtype -= 3;
    --p;

    /* Function Body */
    if (iord == 1) {
	p[1].r = (float)-1., p[1].i = (float)0.;
	strncpy(rtype + 3, "SP", 2);
    } else if (iord == 2) {
	p[1].r = (float)-1.1016013, p[1].i = (float).6360098;
	strncpy(rtype + 3, "CP", 2);
    } else if (iord == 3) {
	p[1].r = (float)-1.0474091, p[1].i = (float).9992645;
	strncpy(rtype + 3, "CP", 2);
	p[2].r = (float)-1.3226758, p[2].i = (float)0.;
	strncpy(rtype + 6, "SP", 2);
    } else if (iord == 4) {
	p[1].r = (float)-.9952088, p[1].i = (float)1.2571058;
	strncpy(rtype + 3, "CP", 2);
	p[2].r = (float)-1.3700679, p[2].i = (float).4102497;
	strncpy(rtype + 6, "CP", 2);
    } else if (iord == 5) {
	p[1].r = (float)-.9576766, p[1].i = (float)1.4711244;
	strncpy(rtype + 3, "CP", 2);
	p[2].r = (float)-1.3808774, p[2].i = (float).7179096;
	strncpy(rtype + 6, "CP", 2);
	p[3].r = (float)-1.502316, p[3].i = (float)0.;
	strncpy(rtype + 9, "SP", 2);
    } else if (iord == 6) {
	p[1].r = (float)-.9306565, p[1].i = (float)1.6618633;
	strncpy(rtype + 3, "CP", 2);
	p[2].r = (float)-1.3818581, p[2].i = (float).9714719;
	strncpy(rtype + 6, "CP", 2);
	p[3].r = (float)-1.5714904, p[3].i = (float).3208964;
	strncpy(rtype + 9, "CP", 2);
    } else if (iord == 7) {
	p[1].r = (float)-.9098678, p[1].i = (float)1.8364514;
	strncpy(rtype + 3, "CP", 2);
	p[2].r = (float)-1.3789032, p[2].i = (float)1.1915667;
	strncpy(rtype + 6, "CP", 2);
	p[3].r = (float)-1.6120388, p[3].i = (float).5892445;
	strncpy(rtype + 9, "CP", 2);
	p[4].r = (float)-1.6843682, p[4].i = (float)0.;
	strncpy(rtype + 12, "SP", 2);
    } else if (iord == 8) {
	p[1].r = (float)-.892871, p[1].i = (float)1.9983286;
	strncpy(rtype + 3, "CP", 2);
	p[2].r = (float)-1.3738431, p[2].i = (float)1.3883585;
	strncpy(rtype + 6, "CP", 2);
	p[3].r = (float)-1.6369417, p[3].i = (float).8227968;
	strncpy(rtype + 9, "CP", 2);
	p[4].r = (float)-1.7574108, p[4].i = (float).2728679;
	strncpy(rtype + 12, "CP", 2);
    }
    nsects = iord - iord / 2;
    *dcvalue = (float)1.;
    return TRUE;
}

/* FUNC DEF */ static complex cmul(a,b)
complex a,b;
{
    complex c;
    c.r = a.r * b.r - a.i * b.i;
    c.i = a.i * b.r + a.r * b.i;
    return c;
}

/* FUNC DEF */ static complex cpowi(a,n)
complex a;
int n;
{
    int i;
    complex c;
    c.i = 0.;
    c.r = 1.;
    for (i = 1; i <= n; ++i) {
	c = cmul(c, a);
    }
    return c;

}
/* FUNC DEF */ static complex jcsqrt(z)
complex z;
{	complex c;
	float x,y,w,r;
	if ((z.r == 0.0) && (z.i == 0.0)) {
		c.r=0.0;
		c.i=0.0;
		return c;
	} else {
		x=fabs(z.r);
		y=fabs(z.i);
		if (x >= y) {
			r=y/x;
			w=sqrt(x)*sqrt(0.5*(1.0+sqrt(1.0+r*r)));
		} else {
			r=x/y;
			w=sqrt(y)*sqrt(0.5*(r+sqrt(1.0+r*r)));
		}
		if (z.r >= 0.0) {
			c.r=w;
			c.i=z.i/(2.0*w);
		} else {
			c.i=(z.i >= 0) ? w : -w;
			c.r=z.i/(2.0*c.i);
		}
		return c;
	}
}

/* FUNC DEF */ static complex conjg(z)
complex z;
{	complex c;
	c.r=z.r;
	c.i = -z.i;
	return c;
}

/* FUNC DEF */ static complex cdiv(a,b)
complex a,b;
{	complex c;
	float r,den;
	if (fabs(b.r) >= fabs(b.i)) {
		r=b.i/b.r;
		den=b.r+r*b.i;
		c.r=(a.r+r*a.i)/den;
		c.i=(a.i-r*a.r)/den;
	} else {
		r=b.r/b.i;
		den=b.i+r*b.r;
		c.r=(a.r*r+a.i)/den;
		c.i=(a.i*r-a.r)/den;
	}
	return c;
}


/** FUNC DEF */ void CALL_JFILT(float *input, int *na, int *iord, char **type, char **aproto,
        double *aa, double *atrbndw, double *afl, double *afh, double *ats, float  *output)
{
   int i;

   int k=*na;
 float a=30.0, trbndw=0.3, fl=59.0, fh=61.0 ;
 float ts;
 float *why;
 double  mean;
char ktype[3], kproto[3];



 a=(float)(*aa);

 trbndw=(float)(*atrbndw);
 fl=(float)(*afl);
 fh=(float)(*afh);
 ts=(float)(*ats);

/* copy trace over to a float array */

   why  = (float *)calloc( k, sizeof(float));
/*     strncpy(kproto,"BU\0",3); */
/*    strncpy(ktype,"LP\0",3); */
/*    fprintf(stderr, "DOING FILTER PARMS:ktype=%2s kproto=%2s\n",ktype, kproto); */
   strncpy(kproto,aproto[0],3);
   strncpy(ktype,type[0],3);
/* fprintf(stderr, "DOING FILTER PARMS:ktype=%2s kproto=%2s\n",ktype, kproto); */

/*
fprintf(stderr, "DOING FILTER PARMS: iord=%d type=%s aproto=%s a=%f trbndw=%f fl=%f fh=%f ts=%f\n",
	      *iord, ktype, kproto, a , trbndw, fl,  fh, ts);
	      */

design(*iord, ktype, kproto, a, trbndw, fl, fh, ts);

/*  for(i=0; i<50; i++) */
/*  { */

/* fprintf(stderr, "%d %lf %lf\n", i, sn[i], sd[i]); */
/*  } */

 mean = 0;
  for(i=0; i<k; i++)
   {
     /*  fprintf(stderr, "%lf\n", input[i]); */
      mean += input[i];
      why[i] = input[i];

   }
  mean /= k;

  for(i=0; i<k; i++)
   {
     /*  fprintf(stderr, "%lf\n", input[i]); */
      
      why[i] -= mean;
      /* if(i<25) */
/*       { */
/* 	 fprintf(stderr, "%f %f\n", why[i], input[i]); */
/*       } */

   }

#if 0
fprintf(stderr, "done, going to apply, k=%d\n", k);
#endif

apply(why, k, 1);
#if 0
  for(i=0; i<25; i++)
   {
      fprintf(stderr, "%lf\n", why[i]);

   }
fprintf(stderr, "done, copy data\n");

#endif


 for(i=0; i< k; i++)
    output[i] = why[i];

 free(why);

}

