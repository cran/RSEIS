/* Routine to do detailed picking on integer seismogram */
/* This routine is stand-alone portable */


/*  an attempt was made to remove all 'adjustments'  */
/*  this is because CRAN was complaining about this  */
/*   JML Sun Sep 15 11:47:17 EDT 2019  */


#include <math.h>
#include <stdlib.h>
#include <stdio.h>
#include <R.h> 

#include "jpiki.h"



/* --------------------------------------------------------------------- */
static int gtcor(float vmax) /* return pick corr from max value
    of ratio curve */
{
    /* System generated locals */
    int ret_val;
    double d_1;

    /* Local variables */
    float temp;

    d_1 = (double) (vmax);
    temp = pow(d_1, GTCOR_EXP) * 15. - 5.;
    if (temp < (float)0.) {
	ret_val = 0;
    } else {
	ret_val = (int) temp;
    }
    return ret_val;
}

/* --------------------------------------------------------------------- */

static int gterr(float vmax) /* ret pick error from max value of ratio curve */
{
    /* System generated locals */
    int ret_val;

    /* Local variables */
    float temp;

    temp = pow((double)(vmax), (double)(GTERR_EXP)) * 30. + 1.;
    if (temp < (float)3.) {
	ret_val = 3;
    } else {
	ret_val = (int) temp;
    }
    return ret_val;
}

/* --------------------------------------------------------------------- */

static int winmax(
float *a,	/* input array (float) */
int ia, int ib,	/* start and end points for search */
int *mxptr,	/* index of maximum value in a */
float *valmx,	/* value of maximum found */
int *mnptr,	/* index of minimum value in a */
float *valmn	/* value of minimum found */
)
{
    /* Local variables */
    int i;

/* find global extremum in array 'a' which does not lie at an */
/*     endpoint.  a local maximum will be missed if it is less than */
/*     one or both endpoints.  In that case, a zero is returned */
/*     for the pointer value of the maximum value. */

    /* Function Body */
    if (ib <= ia) {
	*mxptr = 0;
	*mnptr = 0;
	*valmn = (float)0.;
	*valmx = (float)0.;
	return 0;
    }
    *mxptr = ia;
    *mnptr = ia;
    *valmx = a[ia];
    *valmn = a[ia];
    for (i = ia; i <= ib; ++i) {
	if (a[i] > *valmx) {
	    *mxptr = i;
	    *valmx = a[i];
	}
	if (a[i] < *valmn) {
	    *mnptr = i;
	    *valmn = a[i];
	}
    }
    if (*mxptr == ia || *mxptr == ib) {
	*mxptr = 0;
    }
    if (*mnptr == ia || *mnptr == ib) {
	*mnptr = 0;
    }
    return 0;
}

/* --------------------------------------------------------------------- */
/* Filter input int data array 'din' of length 'ld' using moving
   average filter array 'fil' of length 'lf' and put results in int
   array 'dout'.  All inputs and output are int but products are
   carried out in floating point within routine.
   find wt by calculating area under filter */

static int ifilt(int *din, int ld, int *dout, int *fil, int lf)
{
    /* Local variables */
    int i, j;
    float wt;
    int lf2, sum;
    /* Parameter adjustments */
    /* --fil; */
    /* --dout; */
    /* --din; */

    /* Function Body */
    sum = 0;
    for (i = 0; i < lf; ++i) {
	sum += fil[i];
    }
    wt = (float)1. / (float) sum;
    lf2 = lf / 2;
/* perform actual filtering loop */
    for (i = lf2 ; i < ld-lf2; ++i) {
	sum = 0;
	for (j = 0; j < lf; ++j) {
	    sum += fil[j] * din[i + j - lf2 - 1];
	}
	dout[i] = (float) sum * wt;
    }
/* now fill out endpoints */
    for (i = 0; i < lf2; ++i) {
	dout[i] = dout[lf2 + 1];
	dout[ld - i + 1] = dout[ld - lf2];
    }
    return 0;
}

/* --------------------------------------------------------------------- */
/* find and return polarity at specified pick position
   gtpol = returned polarity;
         = 0 if no polarity determined
         = 1 if up polarity
         = -1 if down polarity */
static int gtpol(int *x, int lx, int pikpos)
{
    /* Initialized data */

    static float thres = (float)4.;
    static int fil[9] = { 1,2,1 };
    static int nf = 3;

    /* System generated locals */
    float r_1;

    /* Local variables */
    int i, delta, ssdel;
    int sdelta, zz[23], zzz[30];
    float ratio, sum1, sum2;

    /* Parameter adjustments */
    /* --x; */

    /* Function Body */
/* If not enough room, return with zero return value */
    if ((lx < 30) || ((lx-pikpos) < 15)) return (0);

/* pull out window of 23 points around pick */
    for (i = -11; i <= 11; ++i) {
	zz[i + 11] = x[pikpos + i-1];
    }
/* filter the seismogram with low pass filter */
    ifilt(zz, 23, zzz, fil, nf);
/* put result back into zz */
    for (i = 1; i <= 23; ++i) {
	zz[i - 1] = zzz[i - 1];
    }
/* compute sum of slopes before and after pick */
    sum1 = (float)0.;
    sum2 = (float)0.;
    for (i = 1; i <= 9; ++i) {
	sum1 += (r_1 = (float) (zz[-i + 12] - zz[-i + 11]), ABS(r_1));
	sum2 += (r_1 = (float) (zz[i + 12] - zz[i + 11]), ABS(r_1));
    }
/* calculate ratio of these slopes to determine if polarity */
/*     will be determined at all (ratio must exceed thres) */
    ratio = (sum2 + (float).1) / (sum1 + (float).1);
    if (ratio < thres) {
	return (0);
    }
/* try finding first maxima after pick then comparing that */
/*     with value at pick to decide polarity */
    i = pikpos;
L145:
    delta = x[i + 1] - x[i];
    if (delta == 0) {
	++i;
	goto L145;
    }
    if (delta < 0) {
	sdelta = -1;
    } else {
	sdelta = 1;
    }
L150:
    if (i - pikpos > 30) {
        return (0);
    }
    delta = x[i + 1] - x[i];
    if (delta < 0) {
	ssdel = -1;
    } else {
	ssdel = 1;
    }
    if (ssdel == sdelta) {
	++i;
	goto L150;
    }
    if (x[i] > x[pikpos]) {
        return (1);
    } else if (x[i] < x[pikpos]) {
        return (-1);
    } else {
        return (0);
    }
}

/* --------------------------------------------------------------------- */
/* note: this routine excised from Dave Harris general
   filter routine "recfil" in SAC; it could be made more efficient
   by not allowing overwrite so that buffers need not be shifted */
static int hpfil(float *data, int *ndata, float *fdata) /* High pass filter */
{
    /* Fixed filter coefficients; 4th order Bessel HP filter */
    /* Corner freq = 0.01 of sample frequency */

    static double a[21] = { 5390958.29333015,-21563833.1733206,
	    32345749.7599809,-21563833.1733206,5390958.29333015 };
    static double b[21] = { 5759441.35423723,-22279798.1925077,
	    32325362.7719655,-20847860.1541335,5042870.22043846 };
    static int order = 5;

    /* System generated locals */
    int i_1;

    /* Local variables */
    double dbuf[21], fbuf[21];
    int i, j, point;
    double out;

    /* Parameter adjustments */
    /* --fdata; */
    /* --data; */

/*  FILTER DATA - FORWARD DIRECTION */

/*  INITIALIZE BUFFER ARRAYS */

    for (i = 1; i <= 21; ++i) {
	dbuf[i - 1] = (float)0.;
	fbuf[i - 1] = (float)0.;
    }

/*  INITIALIZE POINTER */

    point = 0;

/*  LOOP */

L5:
    if (point >= *ndata) {
	goto L6;
    }

/*  FETCH NEW INPUT DATUM */

    dbuf[0] = data[point];

/*  CALCULATE NEW OUTPUT POINT */

    out = a[0] * dbuf[0];
    i_1 = order;
    for (i = 2; i <= i_1; ++i) {
	out = out + a[i - 1] * dbuf[i - 1] - b[i - 1] * fbuf[i - 1];
    }
    fbuf[0] = out / b[0];
    fdata[point] = fbuf[0];

/*  SHIFT BUFFERS */

    i_1 = order;
    for (i = 2; i <= i_1; ++i) {
	j = order + 2 - i;
	fbuf[j - 1] = fbuf[j - 2];
	dbuf[j - 1] = dbuf[j - 2];
    }

/*  UPDATE POINTER */

    ++point;
    goto L5;
L6:
    return 0;
}

/* --------------------------------------------------------------------- */
/*
    Generate front-back ratio curve from the (int) data in "seis"
    and put results in "fbcurv".  Routine uses fast tanking algorithm
    invented by Eric Crosson.  This is the critical characteristic
    function curve that makes the picking algorithm work
    parameters:
	seis = input (int) seismogram
	fbcurv = output ratio curve (float)
	npts = no points in seismogram
	fwlen = front (later in time) window length
	bwlen = back (earlier in time) window length
	    NOTE: both windows are triangle windows
	logflg = if TRUE, deliver the log of the ratio curve
*/
int fbrat(int *seis, float *fbcurv, int npts,
    int fwlen, int bwlen, int logflg)
{
    /* Local variables */
    int bbox, fbox, btri, ftri, i, begpt, endpt, winpt;
    float tscale;
    int beg, end, tmp1, tmp2;


    /* Parameter adjustments - holdover from Fortran */
    /* --fbcurv; */
    /* --seis; */

    /* Function Body */
    beg = 1;
    end = npts;
    begpt = MAX(beg,bwlen);
    endpt = MIN(end, npts-fwlen+1);
    tscale = (float)(bwlen*(bwlen+1)) / (float)(fwlen*(fwlen+1));

    /* Set ratio curve to one before beginning. */

    for (winpt = 0; winpt < begpt-1; ++winpt) {
	fbcurv[winpt] = (float)(1.0);
    }

    /* Set up the windows to start at beginning and compute
   first value. */

    ftri = 0;
    btri = 0;
    fbox = 0;
    bbox = 0;

    for (i = 0; i < fwlen; ++i) {
	ftri += i * (tmp1 = seis[begpt+fwlen-i], ABS(tmp1));
	fbox += (tmp1 = seis[begpt+fwlen-i], ABS(tmp1));
    }

    for (i = 0; i < bwlen; ++i) {
	btri += i * (tmp1 = seis[begpt-bwlen+i], ABS(tmp1));
	bbox += (tmp1 = seis[begpt-bwlen+i], ABS(tmp1));
    }

    /***  rectify curve if numbers are less or equal to zero   ***/

    if (btri > 0 && ftri > 0) {
	fbcurv[begpt] = (float) ftri / (float) btri * tscale;
    } else {
       fbcurv[begpt] = (float)(1.0);
    }

    /* Compute ratio curve using fast tanking algorithm */

    for (winpt = begpt ; winpt < endpt; ++winpt) {
        tmp1 = abs(seis[winpt-1]);
        tmp2 = abs(seis[winpt+fwlen-1]);
	ftri -= fwlen * tmp1;
        fbox += (tmp2 - tmp1);
	ftri += fbox;
	btri -= bbox;
        tmp1 = abs(seis[winpt]);
        tmp2 = abs(seis[winpt-bwlen]);
        bbox += (tmp1 - tmp2);
        btri += bwlen * tmp1;
	if (btri > 0 && ftri > 0) {
	    fbcurv[winpt] = (float) ftri / (float) btri * tscale;
	} else {
	   fbcurv[winpt] = (float)(1.0);
	}
    }

    /* Set ratio curve to one after end */
    for (winpt = endpt ; winpt < npts; ++winpt) {
       fbcurv[winpt] = (float)(1.0);
    }

    /* Comput log if requested */
    if (logflg) {
	for (winpt = 0; winpt < npts; ++winpt) {
	    fbcurv[winpt] = log10((double)fbcurv[winpt]);
	}
    }
    return 0;
}

/* --------------------------------------------------------------------- */
/*
 * epik;	error in pick in index counts
 * polar;	polarity indicator; 0 = none, +1 = up, -1 = down
 * pikwt;	pick weight (0 = on, 9 = off)
 * x[];		input seismogram
 * lx;		length of input seismogram
 * tar;		position of target pick
 */

/** FUNC DEF */ int jpiki(int *epik, int *polar, int *pikwt, int x[], int lx, int tar)
{
    /* System generated locals */
    int ret_val;

    /* Local variables */
    static int bwin = BWIN, fwin = FWIN, rng = RNG;
    float *c, *s;
    int *is, i, begpt, endpt;
    float valmn, valmx, tmp;
    int ptrmn, ptrmx;
    static float offthr = OFFTHR, pikthr = PIKTHR;
FILE  *fopen(const char *filename, const char *mode);
/* FILE *fp, *fopen(); */

/* Pick phase times for seismogram stored in INTEGER array x
   using front/back ratio technique;  'jpiki' returns C style index
   of the pick (starting with 0 as the first point in 'x').
   It is desirable to have the approximate detection point
   in the middle of the data buffer passed to 'jpiki' to avoid
   minor end effects. JPiki is also returns a negative number error flag
   if no pick is found.  Return values are:
     =  the pick index if a normal pick is made (> 0)
        note that 0 (first element) is never an allowable pick
     = -8 if no maximum is located in vicinity of detection point
     = -9 if ratio curve maximum does not meet threshold */

/* allocate space to work */

    /* REprintf(stderr,"starting jpiki\n"); */

  /*   s = (float *)malloc(sizeof(float)*(lx+10)); */
/*     c = (float *)malloc(sizeof(float)*(lx+10)); */
/*     is = (int *)malloc(sizeof(int)*(lx+10)); */


 s = (float *)R_alloc(sizeof(float), (lx+10));
 c = (float *)R_alloc(sizeof(float), (lx+10));
 is = (int *)R_alloc(sizeof(int), (lx+10));




/* initialization of adjustable picking parameters */
    ret_val = 0;
    *epik = 0;
    *polar = 0;
    *pikwt = -1;
/* high pass filter the seismogram after moving it to a float array
   the filter is a 4 pole Bessel high pass filter with high cut
   point at 0.01 of sample frequency (1 Hz for 100 Hz sample freq.).
   Implemented by a recursive algorithm that allows overwriting of input. */

/* note that results of filter operation overwrite s */
    for (i = 0; i < lx; ++i) {
	s[i] = (float) x[i];
    }
    hpfil(s, &lx, s);

/* put absolute value of seismogram into "s" array */
    for (i = 0; i < lx; ++i) {
	s[i] = ABS(s[i]);
    }

/* find f/b ratio curve using new algorithm; results put into c */
/* note: all computation is in floating point from here */

/* put integer & scaled version of seismogram into is */
    for (i = 0; i < lx; ++i) {
        is[i] = (int) (tmp = s[i]*SEIS_SCALE, (tmp>0. ? (tmp+0.5):(tmp-0.5)));
    }
    fbrat(is, c, lx, fwin, bwin, FALSE);

/*
    fp = fopen("jpicki.out","w");
    for (i = 0; i < lx; ++i) { Rprintf(fp, "%d  %f\n", is[i], c[i]); }
    fclose(fp);
*/

/* find pick by looking for maximum value in vicinity of detection point */
    begpt = MAX(0, tar-rng);
    endpt = MIN(lx-1, tar+rng);
    winmax(c, begpt, endpt, &ptrmx, &valmx, &ptrmn, &valmn);
    if (ptrmx != 0 && valmx >= offthr) {
        if (PICKCORR) {
	    ret_val = ptrmx - gtcor(valmx);
        } else {
            ret_val = ptrmx;
        }
	*epik = gterr(valmx);
	*polar = gtpol(&x[1], lx, ptrmx);
	if (valmx >= pikthr) {
	    *pikwt = 0;
	} else {
	    *pikwt = 9;
	}
        goto RETURN;
    }
    if (ptrmx == 0) {
	ret_val = -8;
	*epik = 0;
    } else if (valmx < pikthr) {
	ret_val = -9;
	*epik = 0;
    }
RETURN:
  /*   free(s); */
/*     free(c); */
/*     free(is); */
    return (ret_val);
}


/*******************************/
int dfbrat(double *seis, double *fbcurv, int npts,
    int fwlen, int bwlen, int logflg)
{
    /* Local variables */
  double  bbox, fbox, btri, ftri; 
 int  i, begpt, endpt, winpt;
    double  tscale;
    int beg, end; 
  double tmp1, tmp2;

/*     for(i=0; i<npts; i++) */
/*     { */
/*        REprintf(stderr, "%f\n", seis[i]); */
/* 	       } */

    /* Parameter adjustments - holdover from Fortran */
    /* --fbcurv; */
    /* --seis; */

    /* Function Body */
    beg = 0;
    end = npts-1;
    begpt = MAX(beg,bwlen);
    endpt = MIN(end, npts-fwlen+1);
    tscale = (float)(bwlen*(bwlen+1)) / (float)(fwlen*(fwlen+1));

    /* Set ratio curve to one before beginning. */

    for (winpt = 0; winpt < begpt-1; ++winpt) {
	fbcurv[winpt] = 1.;
    }

    /* Set up the windows to start at beginning and compute
   first value. */

    ftri = 0;
    btri = 0;
    fbox = 0;
    bbox = 0;
    for (i = 0; i < fwlen; ++i) {
	ftri += i * (tmp1 = seis[begpt+fwlen-i], ABS(tmp1));
	fbox += (tmp1 = seis[begpt+fwlen-i], ABS(tmp1));
    }
    for (i = 0; i < bwlen; ++i) {
	btri += i * (tmp1 = seis[begpt-bwlen+i], ABS(tmp1));
	bbox += (tmp1 = seis[begpt-bwlen+i], ABS(tmp1));
    }
    if (btri > 0 && ftri > 0) {
	fbcurv[begpt] = (float) ftri / (float) btri * tscale;
    } else {
	fbcurv[begpt] = 1.0;
    }

    /* Compute ratio curve using fast tanking algorithm */

    for (winpt = begpt ; winpt < endpt; ++winpt) {
        tmp1 = fabs(seis[winpt-1]);
        tmp2 = fabs(seis[winpt+fwlen-1]);
	ftri -= fwlen * tmp1;
        fbox += (tmp2 - tmp1);
	ftri += fbox;
	btri -= bbox;
        tmp1 = fabs(seis[winpt]);
        tmp2 = fabs(seis[winpt-bwlen]);
        bbox += (tmp1 - tmp2);
        btri += bwlen * tmp1;
	if (btri > 0 && ftri > 0) {
	    fbcurv[winpt] = (float) ftri / (float) btri * tscale;
	} else {
	    fbcurv[winpt] = 1.0;
	}
    }

    /* Set ratio curve to one after end */
    for (winpt = endpt ; winpt < npts; ++winpt) {
	fbcurv[winpt] = 1.0;
    }

    /* Comput log if requested */
    if (logflg) {
	for (winpt = 0; winpt < npts; ++winpt) {
	    fbcurv[winpt] = log10((double)fbcurv[winpt]);
	}
    }
    return 0;
}


/** FUNC DEF */ void  CALL_JPIKI(int *epik, int *polar, int *pikwt, int x[], int *ix, int *itar, int  *IND)
{
   int ik, lx, tar;
   lx = *ix;
   tar = *itar;
   ik =  jpiki(epik, polar, pikwt, x , lx ,  tar);
   *IND = ik;
   /* REprintf(stderr, "DONE JPIKI: %d %d %d %d %d\n",ik, *epik, *polar,   *pikwt,*IND ); */
   return;
}


/** int fbrat(int *seis, float *fbcurv, int npts,
    int fwlen, int bwlen, int logflg)  **/

/** FUNC DEF */ 


/*******************************/
/** FUNC DEF */  int djfbrat(double *seis, double *fbcurv, int npts,
    int fwlen, int bwlen, int logflg)
{
    /* Local variables */
   double  bbox, fbox, btri, ftri;
   int i, begpt, endpt, winpt;
   double  tscale;
   int beg, end;
   double tmp1, tmp2;

/*     for(i=0; i<npts; i++) */
/*     { */
/*        REprintf(stderr, "%f\n", seis[i]); */
/* 	       } */

    /* Parameter adjustments - holdover from Fortran */

    /* Function Body */
    beg = 10;
    end = npts;
    begpt = MAX(beg,bwlen-1);
    endpt = MIN(end, npts-fwlen);
    tscale = (float)(bwlen*(bwlen+1)) / (float)(fwlen*(fwlen+1));

    /* Set ratio curve to one before beginning. */

    for (winpt = 0; winpt < begpt; winpt++) {
	fbcurv[winpt] = 1.;
    }

    /* Set up the windows to start at beginning and compute
   first value. */

    ftri = 0;
    btri = 0;
    fbox = 0;
    bbox = 0;
    for (i = 0; i < fwlen; i++) {
	ftri += i * (tmp1 = seis[begpt+fwlen-i], ABS(tmp1));
	fbox += (tmp1 = seis[begpt+fwlen-i], ABS(tmp1));
    }
    for (i = 0; i < bwlen; i++) {
	btri += i * (tmp1 = seis[begpt-bwlen+i], ABS(tmp1));
	bbox += (tmp1 = seis[begpt-bwlen+i], ABS(tmp1));
    }
    if (btri > 0 && ftri > 0) {
	fbcurv[begpt] =  ftri /  btri * tscale;
    } else {
	fbcurv[begpt] = 1.0;
    }

    /* Compute ratio curve using fast tanking algorithm */

    for (winpt = begpt ; winpt < endpt; winpt++) {
        tmp1 = fabs(seis[winpt-1]);
        tmp2 = fabs(seis[winpt+fwlen-1]);
	ftri -= fwlen * tmp1;
        fbox += (tmp2 - tmp1);
	ftri += fbox;
	btri -= bbox;
        tmp1 = fabs(seis[winpt]);
        tmp2 = fabs(seis[winpt-bwlen]);
        bbox += (tmp1 - tmp2);
        btri += bwlen * tmp1;
	if (btri > 0 && ftri > 0) {
	    fbcurv[winpt] =  ftri /  btri * tscale;
	} else {
	    fbcurv[winpt] = 1.0;
	}
    }

    /* Set ratio curve to one after end */
    for (winpt = endpt ; winpt < npts; winpt++) {
	fbcurv[winpt] = 1.0;
    }

    /* Comput log if requested */
    if (logflg) {
	for (winpt = 0; winpt < npts; winpt++) {
	    fbcurv[winpt] = log10((double)fbcurv[winpt]);
	}
    }
    return(1);
}



/** int fbrat(int *seis, float *fbcurv, int npts,
    int fwlen, int bwlen, int logflg)  **/

/** FUNC DEF */ void  CALL_DFBRAT(double *seis, double  *fbcurv, int *npts,
    int *fwlen, int *bwlen, int *logflg)
{
   int ik, lx;
   int fw, bw;

   lx = *logflg;
  
   ik = *npts;
   fw= *fwlen;
   bw = *bwlen;

   djfbrat(seis, fbcurv,  ik,
	 fw,  bw,  lx);

   return;
}

/*****   ######################################################################################   ************/
/*****   ######################################################################################   ************/
/*****   ######################################################################################   ************/
/*****   ######################################################################################   ************/


/** FUNC DEF */  int mjfbrat(double *seis, double *fbcurv, int npts,
    int fwlen, int bwlen, int logflg)
{
    /* Local variables */
   double  bbox, fbox, btri, ftri;
   int i, begpt, endpt, winpt;
   double  tscale;
   int beg, end;
   double tmp1, tmp2;

/*     for(i=0; i<npts; i++) */
/*     { */
/*        REprintf(stderr, "%f\n", seis[i]); */
/* 	       } */

    /* Parameter adjustments - holdover from Fortran */

    /* Function Body */
    beg = 10;
    end = npts;
    begpt = MAX(beg,bwlen-1);
    endpt = MIN(end, npts-fwlen);
    tscale = (float)(bwlen*(bwlen+1)) / (float)(fwlen*(fwlen+1));

    /* Set ratio curve to one before beginning. */

    for (winpt = 0; winpt < begpt; winpt++) {
	fbcurv[winpt] = 1.;
    }

    /* Set up the windows to start at beginning and compute
   first value. */

    ftri = 0;
    btri = 0;
    fbox = 0;
    bbox = 0;
    for (i = 0; i < fwlen; i++) {
	/* ftri += i * (tmp1 = seis[begpt+fwlen-i], ABS(tmp1)); */
	fbox += (tmp1 = seis[begpt+fwlen-i], ABS(tmp1));
    }
    for (i = 0; i < bwlen; i++) {
/* 	btri += i * (tmp1 = seis[begpt-bwlen+i], ABS(tmp1)); */
	bbox += (tmp1 = seis[begpt-bwlen+i], ABS(tmp1));
    }
    if (bbox > 0 && fbox > 0) {
	fbcurv[begpt] =  fbox /  bbox * tscale;
    } else {
	fbcurv[begpt] = 0.0;
    }

    /* Compute ratio curve using fast tanking algorithm */

    for (winpt = begpt ; winpt < endpt; winpt++) {
        tmp1 = fabs(seis[winpt-1]);
        tmp2 = fabs(seis[winpt+fwlen-1]);
	/* ftri -= fwlen * tmp1; */
        fbox += (tmp2 - tmp1);
/* 	ftri += fbox; */
/* 	btri -= bbox; */
        tmp1 = fabs(seis[winpt]);
        tmp2 = fabs(seis[winpt-bwlen]);
        bbox += (tmp1 - tmp2);
       /*  btri += bwlen * tmp1; */
	if (bbox > 0 && fbox > 0) {
	    fbcurv[winpt] =  fbox /  bbox * tscale;
	} else {
	    fbcurv[winpt] = 0.0;
	}
    }

    /* Set ratio curve to one after end */
    for (winpt = endpt ; winpt < npts; winpt++) {
	fbcurv[winpt] = 0.0;
    }

  
    return(1);
}



/** int fbrat(int *seis, float *fbcurv, int npts,
    int fwlen, int bwlen, int logflg)  **/

/** FUNC DEF */ void  CALL_MFBRAT(double *seis, double  *fbcurv, int *npts,
    int *fwlen, int *bwlen, int *logflg)
{
   int ik, lx;
   int fw, bw;

   lx = *logflg;
  
   ik = *npts;
   fw= *fwlen;
   bw = *bwlen;

   mjfbrat(seis, fbcurv,  ik,
	 fw,  bw,  lx);

   return;
}
