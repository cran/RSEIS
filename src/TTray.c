
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <R.h>

#define MIN(a,b) ((a) <= (b) ? (a) : (b))
#define MAX(a,b) ((a) >= (b) ? (a) : (b))
#define ABS(a) ((a) >= (0) ? (a) : -(a))
#define TRUE 1
#define FALSE 0
#define RAD2DEG 57.29577951

static int c__1 = 1;


/* Formal parameters:
	delta = horizontal dist from source to receiver (km)
	hpz = depth of hypocenter below reference surface
	staz = station z coordinate position (+ elev is - staz)
	    note that station z is relative to the zero model reference
	dtdr = partial of travel time w.r.t. horizontal dist
	dtdz = partial of travel time w.r.t. depth (z)
	model_flag = 'P' (P model) or 'S' (S model)
	angle = returned values of takeoff angle in degrees
*/


/*  FUNC DEF  */ double JMLttlvz(double delta, double hpz, double staz, int nlay, double *ztop, double  *vel,
    double *dtdr, double *dtdz, double *angle)
{
    /* ray must get to within "test" dist (km) for direct ray convergence */
    static double test = .0002;
    double d__1, r__1, r__2;

    /* Local variables */
    double sdel, sder, pmax, temp, sumd, dmdp0,
	term1, p, pdir = 0., pref = 0.;
    double term[2000], h[2000]; /* h[] is for local pseudo layers */
    double vmax, p1, offset, sum, tref, tdir, t, ztop_save;
    int lhpz, i, j, lowlr, jlim;
   

  
    /* Find layer containing source; use C index convention */
    for (j = 0, lhpz = nlay-1; j < nlay; ++j) {
	if (hpz <= ztop[j]) {
	    lhpz = j-1;
	    break;
	}
    }
    if (lhpz < 0) lhpz = 0; /* negative source depth may cause this */

    /*
     * Compute interval thicknesses including correction for station
     * elevation, and even negative source depth 
     */
    ztop_save = ztop[0]; /* Save original value */
    ztop[0] = staz; /* adjust first layer top to station elev */
    for (j = 0; j < nlay; ++j) {
	if (j < lhpz) {
	    h[j] = ztop[j+1]-ztop[j];
	} else if (j == lhpz) {
	    h[j] = ABS(hpz - ztop[j]);
	} else {
	    h[j] = ztop[j] - ztop[j-1];
	}
    }
    h[lhpz+1] = ztop[lhpz+1] - MAX(hpz, ztop[0]);
    ztop[0] = ztop_save; /* reset surface layer top to orig value */



    /* Consider possible zero distance; in this case, no ref. calcs. */
    if (ABS(delta) < 1.0e-10) {
	for (j = 0, tdir = 0.0; j < lhpz+1; ++j) {
	    tdir += h[j]/vel[j];
	}
        *angle = 180.;
	*dtdz = 1.0/vel[lhpz];
	if (hpz < staz) *dtdz = -*dtdz; /* rev sign if source above sta */
	*dtdr = 0.0;
	return (tdir);
    }

    /* DIRECT WAVE travel time */
    if (lhpz == 0) { /* In first layer; use trivial straight path solution */
	r__1 = delta;
	r__2 = h[0];
	temp = sqrt(r__1 * r__1 + r__2 * r__2);
	tdir = temp / vel[0];
	pdir = r__1/(vel[0] * temp);
    } else { /* Find maximum velocity */
	vmax = vel[0];
	for (j = 1; j < lhpz+1; ++j) {
	    vmax = MAX(vmax, vel[j]);
	}
	pmax = 1. /  vmax; /* Find max value of p */

	/* Let p0 be p we are seeking */
	/* First use half range search to place p between p0 and pmax */
	p = pmax*.6;
	sdel = 0.0;
	while ( (delta) > p * sdel) {
	    p += (pmax - p)*0.7;
	    for (j = 0, sdel = 0.; j <lhpz+1; ++j) {
		d__1 = p *  vel[j];
		temp = sqrt( 1. - d__1 * d__1);
		sdel +=  (vel[j] * h[j]) / temp;
	    }
	}

	/* Next perform newton convergence from top down; max 20 iterations */
	for (i = 0; i < 20; ++i) {
	    sdel = 0.;
	    sder = 0.;
	    for (j = 0; j <lhpz+1; ++j) {
		d__1 = p *  vel[j];
		temp = sqrt(1. - d__1 * d__1);
		term1 =  (vel[j] * h[j]) / temp;
		sdel += term1;
		d__1 = temp;
		sder += term1 / (d__1 * d__1);
	    }
	    dmdp0 =  (delta) - p * sdel;
	    if (ABS(dmdp0) < test) break;
	    p += dmdp0 / sder;
	}
	/* p has been determined to sufficient accuracy */
	/* Calculate direct wave travel time by summation */
	for (j = 0, sumd = 0.; j <lhpz+1; ++j) {
	    d__1 = p *  vel[j];
	    temp = sqrt(1. - d__1 * d__1);
	    sumd +=  h[j] / ( vel[j] * temp);
	}
	tdir = sumd;
	pdir = p;
    }

    /* REFRACTED RAY CALCULATIONS below here */
    /*
     * init tref to > any allowable trav. time; this takes
     * care of case where refracted time is not computed
     */
    tref = 1.0e20;
    if (lhpz < nlay-1) { /* compute only if source not in halfspace */
    
	/* Find maximum velocity between source and surface */
	for (vmax = vel[0], j = 1; j < lhpz+1; ++j) {
	    vmax = MAX(vel[j],vmax);
	}
    
	for (lowlr = lhpz+1; lowlr < nlay; ++lowlr) {
	    /* check to see if ray exists */
	    if (vel[lowlr] <= vmax) continue;
	    
	    vmax = vel[lowlr];
	    jlim = lowlr - 1;
	    /* calculate offset distance */
	    p1 = 1.0 / vel[lowlr];
	    for (j = 0, sum = 0.; j < lhpz+1; ++j) {
		r__1 = p1 * vel[j];
		term[j] = sqrt(1.-r__1*r__1) + 1.0e-10;
		sum += h[j] * vel[j] / term[j];
	    }
	    for (j = lhpz; j < lowlr; ++j) {
		r__1 = p1 * vel[j];
		term[j] = sqrt(1. - r__1 * r__1) + 1e-10;
		sum += 2. * h[j+1] * vel[j]/term[j];
	    }
	    offset = sum * p1;
	    if (delta - offset < 0.) continue;
	    
	    /* Calculate refraction path travel time for lowlr */
	    for (sum = 0., j = 0; j < lhpz+1; ++j) {
		sum += h[j] * term[j] / vel[j];
	    }
	    /*Loop to compute offset time both ways from source to refractor*/
	    for (j = lhpz; j < lowlr; ++j) {
		sum += h[j+1] * 2. * term[j] / vel[j];
	    }
	    if ((t = delta * p1 + sum) < tref) {
		tref = t;
		pref = p1;
	    }
	}
    }
    /* At this point, tdir = direct wave tt; tref = refracted wave tt */
    if (tdir < tref) { /* Direct wave has minimum travel time */
        *angle = 180. - asin(pdir * vel[lhpz])* RAD2DEG;
	*dtdr = pdir;
	temp = vel[lhpz];
	temp *= temp;
	*dtdz = sqrt((1./temp) - pdir*pdir);
	if (hpz < staz) *dtdz = -*dtdz; /* rev sign if source above sta */
	return(tdir);
    } else { /* Refracted wave has minimum travel time */
        *angle = asin(pref * vel[lhpz])*RAD2DEG;
	*dtdr = pref;
	temp = vel[lhpz];
	temp *= temp;
	*dtdz = -sqrt((1./temp) - pref*pref);
	return(tref);
    }
}





/* FUNC DEF */ int fcopy(double *x, double *y, int *n)
{
    static int  i, in;
     /* ... copy vector y into vector x (both of length n) */
    /* Function Body */
    in  = *n;
    for (i = 0 ; i <  in; i++) {
	x[i] = y[i];
    }
    return 0;
} 



double Adotf_(double *x, int *ix, double *y, int *iy, int *n)
{
    /* System generated locals */
    int i__1;
    double  ret_val;

    /* Local variables */
    static double  c;
    static int i, k, l;
    static double   s, t1, t2;
    static int ks, ls;

/* --- single precision dot product with accuracy enhancements */
/* --- x = vector 1 */
/*     ix = skip factor for vector 1 */
/*     y = vector 2 (input) */
/*     iy = skip factor for vector 2 */
/*     n = number of terms to sum in inner product */
    /* Parameter adjustments */
    --y;
    --x;

    /* Function Body */
    s = 0.;
    c = 0.;
    ks = *ix;
    ls = *iy;
    k = 1;
    l = 1;
    i__1 = *n;
    for (i = 1; i <= i__1; ++i) {
	t1 = c + x[k] * y[l];
	k += ks;
	l += ls;
	t2 = s + t1;
	c = s - t2 + t1;
	s = t2;
/* L110: */
    }
    ret_val = s + c;
    return ret_val;
} /* Adotf_ */

/*******************************************************************/
/*******************************************************************/

int revarr(int *n, double *a)
{
    /* System generated locals */
    int i__1;

    /* Local variables */
    static double temp;
    static int i, index, lim;

/* --- revarr reverses or inverts the order of elements in array a */
/*     for use by subroutine ttinvr */
    /* Parameter adjustments */
    --a;

    /* Function Body */
    lim = *n / 2;
    i__1 = lim;
    for (i = 1; i <= i__1; ++i) {
        temp = a[i];
        index = *n - i + 1;
        a[i] = a[index];
        a[index] = temp;
/* L100: */
    }
    return 0;
} /* revarr_ */

/*******************************************************************/
/*******************************************************************/


/*  FUNC DEF  */ double ccinv1(double *r, double *hpz, int *nly, double *ttop, double *slness, int *jerr, int *nnod, double *znod, double *rnod , int *iret1, int *iret2)
{
    /* System generated locals */
    int i__1, i__2;
    double ret_val=0, r__1, r__2;
    double d__1, d__2;

    /* Builtin functions */

    /* Local variables */
    static double sflg;
    static double dtdr, *dtds;
    static double pold;
    static double sder;
    static int lidx;
    static double smin, pmax;
    static int lhpz;
    static double rsum, dmdp0, testconv, *d, *h;
    static int i, j;
    static double p;
     static double dtemp, ttime;
    static int lowlr;
    static double t1, t2, t3;
    static int istrt;
    static double tt;
    static int indpth;
    static double den;
    static int lim, lay;
    static double *top, sum;
    static int layereq;

       int   nnodes, nvellay;


/* --- ccinv1 calculates minimum traveltime in arbitrary layered model. */
/*     source is depth -hpz- and receiver is offset distance -r- from */
/*     source.  model consists of horizontal, plane parallel layers */
/*     (n-1 layers over homogeneous halfspace). */
/* --- model is specified by */
/*       number of layers (nly) */
/*       slowness in each layer (slo array) */
/*       depth to layer top (top array) */
/* --- routine also calculates array containing nodes of minimum */
/*     traveltime path, an array of partial derivatives of travel */
/*     time with respect to all model slownesses, and partial derivatives 
*/
/*     with respect to source depth and range */
/* --- program modified from ttlvz and including some conventions */
/*     used by chee wu chou for use in three-dimensional traveltime */
/*     calculations */
/* --- ccinv1 calls subroutine 'revarr' */

/*       by robert s. crosson */
/*       geophysics program */
/*       university of washington */
/*       seattle, washinton 98195 */
/*       april, 1979 */

/*   I was getting some sqrt domain errors on the calculation */
/*     of dtdz so I removed it from here. Also the calculation of t1 right
*/
/*      before it.  I don't use these so there is no loss, (I hope) */
/*         Jonathan Lees 8/12/91 */

/*      include '../Inv/slocom' */
/*     block specifying currentl set slowness model */
/*      common /slness/ nly,slness,top */
/* ----------------------------------------------------------- */
/*          include '../Src2/nodecom */
/*      common /nodes/ lhpz,indpth,lowlr,nnod */
/*      common /dodes/ znod,rnod */
/*      common /ttdrv/ dtdr,dtdz,dtds(70) */
/*      common /ttconv/ testconv */

/* initialized in ../Inv/slocom  double c1(32),c2(32),znod(32),rnod(32) */
/* ----------------------------------------------------------- */
/* ---------------------------------------------------------- */
    /* Parameter adjustments */



    


    --rnod;
    --znod;
    --slness;
    --ttop;

	nvellay = *nly;
	nnodes = nvellay*2+2;

	


   	dtds = (double *) calloc(nnodes, sizeof(double));
	d = (double *) calloc(nnodes, sizeof(double));
   	h = (double *) calloc(nnodes, sizeof(double));
     	top = (double *) calloc(nnodes, sizeof(double));
 




    /* Function Body */
    *jerr = 0;
/* 	jerr is a flag signifying all okay if = 0 */
/* --- note following conventions */
/*       indpth = 2 if path is refracted path */
/*              = 1 if path is direct path */
/*       lowlr = index of lowest layer penetrated by raypath */
/* --- testconv is convergence crierion for direct ray path calculation */
/*    when the direct ray gets within distance -testconv- of -r-, iteration*/
/*     stops. */
/* --- if source is above receiver, interchange elevations of source */
/*     and receiver and set flag -sflg- */
/* ---------------------------------------------------------- */


    

    fcopy(top, &ttop[1], nly);

    sflg = 1.0;
    if (*hpz >= top[0]) {
	goto L95;
    }
/* for an event which is above ground: (hpz is negative) */
    t1 = *hpz;
    *hpz = top[0];
    top[0] = t1;
    sflg = (-1.0);
L95:
    

/*        write(0,*) 'sflg = ',sflg */
/* --- locate layer containing hypocenter and store index in lhpz. */
/*     note that if hypocenter on a boundary, it is placed in overlying */
/*     layer for refraction path calculation */
    i__1 = *nly;
    for (i = 1; i <= i__1; ++i) {
	if (*hpz <= top[i - 1]) {
	    goto L110;
	}
/* L100: */
    }
    lhpz = *nly;
    goto L120;

L110:
    lhpz = i - 1;
    if (lhpz == 0) {
	lhpz = 1;
    }
L120:
/* 	write(0,*)'lhpz=',lhpz */
    layereq = lhpz;
/* --- assign layer thicknesses to internal array h only for those */
/*     layers above source. */
    lim = lhpz - 1;
    i__1 = lim;
    for (i = 1; i <= i__1; ++i) {
	h[i - 1] = top[i] - top[i - 1];
/* L130: */
    }
    h[lhpz - 1] = (r__1 = *hpz - top[lhpz - 1], fabs(r__1));
/* --- initialize for refraction path calculations */
    indpth = 2;
    ttime = 1.0e10;
/* --- if source in halfspace, skip refracted ray calculation and */
/*     go to direct ray calculation */
    if (lhpz == *nly) {
	goto L220;
    }
/* --- prepare for loop which examines for all possible refracted */
/*     paths (paths going below source) */
/* --- find maximum velocity (minimum slowness) from source to surface */
    smin = 1.0e30;
    i__1 = lhpz;
    for (i = 1; i <= i__1; ++i) {
/* Computing MIN */
	r__1 = slness[i];
	smin = MIN(r__1,smin);
/* L140: */
    }
/* --- no refraction path can exist off any deeper layer which has */
/*     slowness greater than all slownesses between layer and surface */
/*     (this would be low velocity zone) */
    istrt = lhpz + 1;
    i__1 = *nly;
    for (lay = istrt; lay <= i__1; ++lay) {
/* --- check to see if low velocity criterion excludes refracted path 
*/
	if (slness[lay] >= smin) {
	    goto L210;
	}
	p = slness[lay];
	smin = p;
/* --- calculate partial slant distances, d(k) */
	lim = lay - 1;
	i__2 = lim;
	for (i = 1; i <= i__2; ++i) {
	    t1 = p / slness[i];
/* Computing 2nd power */
	    d__1 = t1;
	    t2 = d__1 * d__1;
	    t3 = sqrt(1.0 - t2);
	    d[i - 1] = (top[i] - top[i - 1]) * 2.0 / t3;
/* L150: */
	}
/* --- now calculate that part of slant distance from source to surfac
e */
/*     using h array, and subtract out appropriate part from twice abo
ve */
	i__2 = lhpz;
	for (i = 1; i <= i__2; ++i) {
	    t1 = p / slness[i];
/* Computing 2nd power */
	    d__1 = t1;
	    t2 = d__1 * d__1;
	    t3 = sqrt(1.0 - t2);
	    d[i - 1] -= h[i - 1] / t3;
/* L160: */
	}
/* --- now d(i) array contains correct distribution of partial slant 
*/
/*     distances for all layers.  calculate total offset distance */
	sum = 0.0;
	i__2 = lim;
	for (i = 1; i <= i__2; ++i) {
	    t1 = p / slness[i];
	    sum = d[i - 1] * t1 + sum;
/* L170: */
	}
/* --- sum now contains total offset distance. compare against r */
/* 	write(0,*)'sum=',sum */
/* ---------------------------------------------------------- */
	if (sum - *r <= 0.0) {
	    goto L180;
	} else {
	    goto L210;
	}
L180:
/* --- refraction path possible, assign final partial distance in lay 
*/
	d[lay - 1] = *r - sum;
/* --- can now calculate total travel time by inner product of slant 
*/
/*     path distances and slowness vector */
	tt = Adotf_(d, &c__1, &slness[1], &c__1, &lay);
/* --- if travel time is a minimum, save partial path distances in */
/*     derivative array and go on to next possible refracted path */
	if (ttime <= tt) {
	    goto L210;
	}
	ttime = tt;
	i__2 = *nly;
	for (i = lay; i <= i__2; ++i) {
	    dtds[i - 1] = 0.0;
/* L190: */
	}
	i__2 = lay;
	for (i = 1; i <= i__2; ++i) {
	    dtds[i - 1] = d[i - 1];
/* L200: */
	}
	dtdr = p;
	lowlr = lay;
/* --- note that we can construct final node array from partial */
/*     derivative array, as well as reconstructing d array */
L210:
	;
    }
/* ---------------------------------------------------------- */
/* 	write(0,*)'lowlr,dtds(array)',lowlr,dtds */

   


/* --- refraction path calculations are now completed. variable lowlr */
/*     contains the minimum time raypath bottom layer index and */
/*     dtds array contains the partial slant distances in all layers */
/* --- proceed to calculate direct ray path for comparison. */
L220:
    i__1 = *nly;
    for (i = 1; i <= i__1; ++i) {
	d[i - 1] = 0.0;
/* L230: */
    }
/* --- if source in layer 1, use homogeneous layer calcultion */
    if (lhpz > 1) {
	goto L240;
    }
/* Computing 2nd power */
    r__1 = *r;
/* Computing 2nd power */
    r__2 = h[lhpz - 1];
    d[0] = sqrt(r__1 * r__1 + r__2 * r__2);
    p =0.0;
    if (d[0] == 0.0) {
	goto L320;
    }
    p = slness[lhpz] * *r / d[0];
    goto L320;
/* --- proceed to calculate direct path by iteration */
L240:
/* --- find minimum slowness between surface and source */
    smin = slness[1];
    i__1 = lhpz;
    for (i = 2; i <= i__1; ++i) {
/* Computing MIN */
	r__1 = smin, r__2 = slness[i];
	smin = MIN(r__1,r__2);
/* L250: */
    }
/* --- find maximum permissable value of p (p must lay between 0. */
/*     and pmax) */
    pmax = smin;
    p = pmax * 0.5;
/* --- find a value of p which is greater than true p by half range */
/*     divide and test to see when ray emerges at distance greater */
/*     than r */
/* 	write(0,*)'looking for p greater than true p' */
    pold = p;
L260:
    p = (p + pmax) / 2.0;
/*    there is a problem here: program gets into infinite loops: */
/*    need to test for p not changing from one iter. to next */
    if (p == pold) {
	goto L9500;
    }
    pold = p;
/* 	write(0,*)'****************************' */
/* 	write(6,*)'****************************' */
/* 	write(6,*) 'beg loop,p',p */
    sum = 0.;
    i__1 = lhpz;
    for (i = 1; i <= i__1; ++i) {
	t1 = p / slness[i];
/* Computing 2nd power */
	d__1 = t1;
	t2 = d__1 * d__1;
	t3 = sqrt(1. - t2);
	d[i - 1] = h[i - 1] / t3;
	sum = d[i - 1] * t1 + sum;
/* 	write(6,*)'i t1 t2 t3 di hi sum',i,t1,t2,t3,d(i),h(i),sum */
/* 	write(0,*)'sum,r,t1,t2,t3,and di hi',sum,r,t1,t2,t3 */
/* 	write(0,*) d(i),h(i), i */
/* L270: */
    }

    

/* 	write(0,*)'outer loop; sum,r,t1,t2,t3',sum,r,t1,t2,t3 */
    if ((r__1 = *r - sum) < 0.) {
	goto L280;
    } else if (r__1 == 0) {
	goto L310;
    } else {
	goto L260;
    }
L280:
/* --- perform newton convergence on true p from top down.  permit */
/*     no more than 20 iterations by nesting in do loop */
    for (i = 1; i <= 20; ++i) {
	sum = 0.;
	sder = 0.;
	i__1 = lhpz;
	for (j = 1; j <= i__1; ++j) {
	    t1 = p / slness[j];
/* Computing 2nd power */
	    d__1 = t1;
	    t2 = d__1 * d__1;
	    t3 = sqrt(1. - t2);
	    d[j - 1] = h[j - 1] / t3;
	    sum = d[j - 1] * t1 + sum;
/* Computing 3rd power */
	    d__1 = t3, d__2 = d__1;
	    den = d__2 * (d__1 * d__1) * slness[j];
	    sder = h[j - 1] / den + sder;
/* L290: */
	}
	dmdp0 = *r - sum;
/* ---------------------------------------------------------- */
/* 	write(0,*)'this is testconv and dmdp0:',testconv,dmdp0 */
/* ---------------------------------------------------------- */
	if (fabs(dmdp0) < testconv) {
	    goto L310;
	}
	p = dmdp0 / sder + p;
/* L300: */
    }
L310:

    

/* 	write(0,*)'p is found!' */
/* --- p has now been found to sufficient accuracy, calculate direct */
/*     wave travel time and compare with refracted time to find minimum */
L320:
    tt = Adotf_(d, &c__1, &slness[1], &c__1, &lhpz);
    if (tt > ttime) {
	goto L350;
    }


   
/* --- following code assumes direct wave time is minimum. variables */
/*     are set and nodes are filled on this basis */
    ttime = tt;
    indpth = 1;
    *nnod = lhpz + 1;
    znod[1] = *hpz;
    rnod[1] = 0.;
    znod[*nnod] = top[0];
    rnod[*nnod] = *r;
/* --- accumulate progressive range increments from source */
    rsum = 0.;
    lidx = lhpz;
    lim = lhpz - 1;
    i__1 = lim;
    for (i = 1; i <= i__1; ++i) {
	lidx = lhpz - i + 1;
	
	/* if(slness[lidx]==0) */
/* 	{ */

/* 	} */
       
	rsum = d[lidx - 1] * p / slness[lidx] + rsum;
	
/* 	if(!(rsum>=0)) */
/* 	{ */
/* 	   REprintf( "problem with slness: layer=%d\n", lidx); */
/* 	} */
       
	rnod[i + 1] = rsum;

	znod[i + 1] = top[lidx - 1];
/* L330: */
    }
    dtdr = p;
/*      t1 = (p/slness(lhpz))**2 */
/*      dtdz = sqrt(1.-t1)*slness(lhpz) */
    lowlr = lhpz;
    i__1 = *nly;
    for (i = 1; i <= i__1; ++i) {
	dtds[i - 1] = d[i - 1];
/* L340: */
    }
    ret_val = ttime;

    /*  REprintf( "a ccinvr rnod 1 nnod\n");  */

/*      write(0,*)'a ccinvr rnod 1 nnod',rnod(1),rnod(nnod) */

/****  I made some changes here to ge tthis to work with
****   single layer straight line ray paths...I do not knwo why this was a problem in the past)
****   JML Sat Nov 26 16:35:39 EST 2005
****/



    if(!(rnod[*nnod]>0))
    {
      
       /* for (i = 1; i <= *nnod; ++i)  */
/*        { */
/* 	  REprintf( "ccinvr a rnod=%d %f %f\n" ,i, rnod[i], ret_val ); */
	  
/*        } */
      
       ret_val = -1;
       *iret1 = lowlr-1;
       *iret2 = indpth;
       return ret_val;
    }
    /* REprintf( "a ccinvr rnod 1 nnod %f %f\n", rnod[0],rnod[*nnod-1]); */
    if (sflg == 1.0) {
	free(dtds);
	free(h);
	free(d);
	free(top);
       *iret1 = lowlr-1;
       *iret2 = indpth;
	return ret_val;
    }
    t1 = *hpz;
    *hpz = top[0];
    top[0] = t1;
/*      dtdz = -dtdz */
    revarr(nnod, &rnod[1]);
    t1 = rnod[1];
    i__1 = *nnod;
    for (i = 1; i <= i__1; ++i) {
	rnod[i] = t1 - rnod[i];
/* 	write(0,*)'ccinv i  rnod ',i,rnod(i) */
	/* REprintf( "ccinv i  rnod %d %f\n", i,rnod[i]); */

/* L345: */
    }
    revarr(nnod, &znod[1]);
/*      write(0,*)'b ccinvr rnod 1 nnod',rnod(1),rnod(nnod) */
      /* REprintf("b ccinvr rnod 1 nnod %f %f\n", rnod[0], rnod[*nnod-1]);   */
	free(dtds);
	free(h);
	free(d);
	free(top);
       *iret1 = lowlr-1;
       *iret2 = indpth;
    return ret_val;
L350:
/* --- following code assumes refraction path is minimum time. */
/*     nodes and derivatives are assigned on this basis */
    p = dtdr;
/* 	  I think this is a bug....nnod should be zero here: */
/* ..............This is not clear */
/* 9/15/91 - I had alot of problems with this "fix" */
/* so I turned it back to where it was in the original */
/*   ccinvr....I don't recall what the problem was */
/*   or what the manifestations...but I think it really */
/*  does better this way....I do recall having tons of problems */
/*  with nnod and the rnod and znod vectors - but that */
/*     had to do with writing over vectors */
/*  at any rate this is back to where it was */
/* -------------------------------- */
/* 9/16/91 I am still having problems with this part I think */

    *nnod = 1;
/*      nnod = 0 */
/*  this is the "fix" which I am turning off now... */
/*      write(0,*)hpz,top(lhpz+1) */
/*      if(hpz.eq.top(lhpz+1))then */
/* 	     nnod = 0 */
/* 	     else */
/* 	     nnod = 1 */
/* 	     endif */
    znod[1] = *hpz;
    rnod[1] = 0.0;
/* --- reconstruct correct d(i) array from dtds array */
    i__1 = *nly;
    for (i = 1; i <= i__1; ++i) {
	d[i - 1] = dtds[i - 1];
/* L360: */
    }
/* --- calculate offset to first layer below source */
    t1 = top[lhpz] - top[lhpz - 1];
    t2 = top[lhpz] - *hpz;
    dtemp = t2 / (t1 + t2) * d[lhpz - 1];
    rsum = 0.0;
/* --- now assign nodes for points below source */
    lim = lowlr - 1;
    i__1 = lim;
    for (i = lhpz; i <= i__1; ++i) {
	rsum = dtemp * p / slness[i] + rsum;
	++(*nnod);
	rnod[*nnod] = rsum;
	znod[*nnod] = top[i];
	dtemp = d[i] / 2.0;
/* L370: */
    }
/* --- assign nodes for part of path in lowlr */
    rsum += d[lowlr - 1];
    ++(*nnod);
    rnod[*nnod] = rsum;
    znod[*nnod] = top[lowlr - 1];
/* --- work way back upgoing side of raypath */
    lim = lowlr - 1;
    i__1 = lim;
    for (i = 1; i <= i__1; ++i) {
	lidx = lowlr - i;
	t1 = p / slness[lidx];
/* Computing 2nd power */
	d__1 = t1;
	t2 = d__1 * d__1;
	t3 = sqrt(1.0 - t2);
	rsum = (top[lidx] - top[lidx - 1]) * t1 / t3 + rsum;
	++(*nnod);
	rnod[*nnod] = rsum;
	znod[*nnod] = top[lidx - 1];
/* L380: */
    }
/* --- all nodes assigned, now assign derivatives */
/*      write(0,*)' in ccinv layereq=',layereq */
/*      write(0,*)' in ccinv lhpz=',lhpz */
/*      lhpz = layereq */
/*      t1 = (p/slness(lhpz))**2 */
/*      dtdz = -sqrt(1.-t1)*slness(lhpz) */
    ret_val = ttime;
/*      write(0,*)'c ccinvr rnod 1 nnod',rnod(1),rnod(*nnod) */
    if (sflg == 1.0) {
	free(dtds);
	free(h);
	free(d);
	free(top);
       *iret1 = lowlr-1;
       *iret2 = indpth;
	return ret_val;
    }
    t1 = *hpz;
    *hpz = top[0];
    top[0] = t1;
    revarr(nnod, &rnod[1]);
    t1 = rnod[1];
    i__1 = *nnod;
    for (i = 1; i <= i__1; ++i) {
	rnod[i] = t1 - rnod[i];
/* 	write(0,*)'ccinv i  rnod ',i,rnod(i) */
	
/* L385: */
    }
    revarr(nnod, &znod[1]);
/*      write(0,*)'d ccinvr rnod 1 nnod',rnod(1),rnod(nnod) */
    /* REprintf( "d ccinvr rnod 1 nnod %f %f\n", rnod[0],rnod[*nnod-1]); */

	free(dtds);
	free(h);
	free(d);
	free(top);
       *iret1 = lowlr-1;
       *iret2 = indpth;
    return ret_val;
/*   used incase of faulty execution */
L9500:
    *jerr = -1;
/*      write(0,*)'error in ccinv1: p=pold jerr = -1' */
    REprintf( "error in ccinv1: p=pold jerr = -1 \n");
	free(dtds);
	free(h);
	free(d);
	free(top);
       *iret1 = lowlr-1;
       *iret2 = indpth;
    return ret_val;
} /* ccinv1_ */

/***********************************************************************************/
/***********************************************************************************/
/***********************************************************************************/

/** FUNC DEF */ void   CALL_DTTmany(int *n, double *delta, double *hpz, double *staz, int *nlay, double *ztop, double  *vel,
				 double *dtdr, double *dtdz, double *angle, double *outt)
{
  /************  loop call to travel time calculations  *****/
  /***   delta is a vector of N distance, staz is a vector of N station elevations relative to vel model ********/

  int i, N;
  double tt;
  double indelta, inhpz,  instaz;
  double  ddr, ddz, dang, dist;

  int inlay;
  
  inhpz=*hpz;
  
  inlay=*nlay;

  N = *n;
  
  for(i=0; i<N; i++)
    {
      dist = delta[i];
      instaz= staz[i];

      tt = JMLttlvz(dist, inhpz, instaz, inlay , ztop ,  vel, &ddr, &ddz, &dang);

      dtdr[i]=ddr;
      dtdz[i]=ddz;
      angle[i]=dang;
      outt[i] = tt;

    }
  return;
}


/** FUNC DEF */ void   CALL_DTT1(double *delta, double *hpz, double *staz, int *nlay, double *ztop, double  *vel,
				 double *dtdr, double *dtdz, double *angle, double *outt)
{

  double tt;
  double indelta, inhpz,  instaz;
  int inlay;
  indelta= *delta;
  inhpz=*hpz;
  instaz= *staz;
  inlay=*nlay;

  tt = JMLttlvz(indelta, inhpz, instaz, inlay , ztop ,  vel, dtdr, dtdz, angle);
  *outt = tt;

  return;
}

/** FUNC DEF */ void   CALL_DTTray(double *delta, double *hpz, double *staz, int *nlay, double *ztop, double  *vel,
				   double *dtdr, double *dtdz, double *angle, double *outt, int *nnod, double *znod, double *rnod)
{

  double tt;
  double indelta, inhpz,  instaz;
  int inlay;

  int jerr, lolev, indpth;
  int i, j;

  indelta= *delta;
  inhpz=*hpz;
  instaz= *staz;
  inlay=*nlay;

  /*  tt = JMLttlvz(indelta, inhpz, instaz, inlay , ztop ,  vel, dtdr, dtdz, angle); */
 
  tt = ccinv1(delta, hpz, nlay, ztop,  vel, &jerr, nnod, znod, rnod, &lolev, &indpth);

  if((jerr<0))
    {
      
      REprintf( "ERROR:jtrace tt<0: nnod = %d  dis=%f hypoz=%lf  newlay=%d  tt=%lf JERR=%d\n", 
	      *nnod, indelta,  inhpz, lolev, tt,  jerr);
    }
  
  j = *nnod;

  for(i=0; i<j; i++)
    {
      REprintf( "%lf, %lf\n", znod[i], rnod[i]);

    }

  *outt = tt;
  return;
}

