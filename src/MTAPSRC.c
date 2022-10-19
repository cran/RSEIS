
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <stdlib.h>
#include <R.h> 


#define NRANSI
#include "jl.h"


#define perr(x,y)  (REprintf( x , y))
#define prbl (REprintf("\n"))



#define abs(x) ((x) >= 0 ? (x) : -(x))
#define dabs(x) (double)abs(x)
#define min(a,b) ((a) <= (b) ? (a) : (b))
#define max(a,b) ((a) >= (b) ? (a) : (b))
#define dmin(a,b) (double)min(a,b)
#define dmax(a,b) (double)max(a,b)

/********************************************************/
/********************************************************/
/********************************************************/


/* ./ add name=tinvit */

/*     ------------------------------------------------------------------ */

/* Subroutine */ 
/** FUNC DEF */ int jtinvit_(int *nm, int *n, double *d, double *e, double *e2, int *m, double *w, int *ind, double *z, int *ierr, double *rv1, double *rv2, 
	double *rv3, double *rv4, double *rv6)
{
    /* Initialized data */

    static double machep = 1.25e-15;

    /* System generated locals */
    int z_dim1, z_offset, i__1, i__2, i__3;
    double d__1, d__2;

    /* Builtin functions */
    double sqrt(double);

    /* Local variables */
    static double norm;
    static int i, j, p, q, r, s;
    static double u, v, order;
    static int group;
    static double x0, x1;
    static int ii, jj, ip;
    static double uk, xu;
    static int tag, its;
    static double eps2, eps3, eps4;

     static double rtem;


/*     this subroutine is a translation of the inverse iteration tech- */
/*     nique in the algol procedure tristurm by peters and wilkinson. */
/*     handbook for auto. comp., vol.ii-linear algebra, 418-439(1971). */

/*     this subroutine finds those eigenvectors of a tridiagonal */
/*     symmetric matrix corresponding to specified eigenvalues, */
/*     using inverse iteration. */

/*     on input: */

/*        nm must be set to the row dimension of two-dimensional */
/*          array parameters as declared in the calling program */
/*          dimension statement; */

/*        n is the order of the matrix; */

/*        d contains the diagonal elements of the input matrix; */

/*        e contains the subdiagonal elements of the input matrix */
/*          in its last n-1 positions.  e(1) is arbitrary; */

/*        e2 contains the squares of the corresponding elements of e, */
/*          with zeros corresponding to negligible elements of e. */
/*          e(i) is considered negligible if it is not larger than */
/*          the product of the relative machine precision and the sum */
/*          of the magnitudes of d(i) and d(i-1).  e2(1) must contain */
/*          0.0d0 if the eigenvalues are in ascending order, or 2.0d0 */
/*          if the eigenvalues are in descending order.  if  bisect, */
/*          tridib, or  imtqlv  has been used to find the eigenvalues, */
/*          their output e2 array is exactly what is expected here; */

/*        m is the number of specified eigenvalues; */

/*        w contains the m eigenvalues in ascending or descending order; 
*/

/*        ind contains in its first m positions the submatrix indices */
/*          associated with the corresponding eigenvalues in w -- */
/*          1 for eigenvalues belonging to the first submatrix from */
/*          the top, 2 for those belonging to the second submatrix, etc. 
*/

/*     on output: */

/*        all input arrays are unaltered; */

/*        z contains the associated set of orthonormal eigenvectors. */
/*          any vector which fails to converge is set to zero; */

/*        ierr is set to */
/*          zero       for normal return, */
/*          -r         if the eigenvector corresponding to the r-th */
/*                     eigenvalue fails to converge in 5 iterations; */

/*        rv1, rv2, rv3, rv4, and rv6 are temporary storage arrays. */

/*     questions and comments should be directed to b. s. garbow, */
/*     applied mathematics division, argonne national laboratory */

/*     ------------------------------------------------------------------ 
*/

/*     :::::::::: machep is a machine dependent parameter specifying */
/*                the relative precision of floating point arithmetic. */
/*                machep = 16.0d0**(-13) for long form arithmetic */
/*                on s360 :::::::::: */
/*  for f_floating dec fortran */
/*      data machep/1.1d-16/ */
/*  for g_floating dec fortran */
    /* Parameter adjustments */
    --rv6;
    --rv4;
    --rv3;
    --rv2;
    --rv1;
    --e2;
    --e;
    --d;
    z_dim1 = *nm;
    z_offset = z_dim1 + 1;
    z -= z_offset;
    --ind;
    --w;

    /* Function Body */

    *ierr = 0;
    if (*m == 0) {
	goto L1001;
    }
    tag = 0;
    order = 1. - e2[1];
    q = 0;
/*     :::::::::: establish and process next submatrix :::::::::: */
L100:
    p = q + 1;

    i__1 = *n;
    for (q = p; q <= i__1; ++q) {
	if (q == *n) {
	    goto L140;
	}
	if (e2[q + 1] == 0.) {
	    goto L140;
	}
/* L120: */
    }
/*     :::::::::: find vectors by inverse iteration :::::::::: */
L140:
    ++tag;
    s = 0;

    i__1 = *m;
    for (r = 1; r <= i__1; ++r) {
	if (ind[r] != tag) {
	    goto L920;
	}
	its = 1;
	x1 = w[r];
	if (s != 0) {
	    goto L510;
	}
/*     :::::::::: check for isolated root :::::::::: */
	xu = 1.;
	if (p != q) {
	    goto L490;
	}
	rv6[p] = 1.;
	goto L870;
L490:
	norm = (d__1 = d[p], abs(d__1));
	ip = p + 1;

	i__2 = q;
	for (i = ip; i <= i__2; ++i) {
/* L500: */
	    norm = norm + (d__1 = d[i], abs(d__1)) + (d__2 = e[i], abs(d__2));
	}
/*     :::::::::: eps2 is the criterion for grouping, */
/*                eps3 replaces zero pivots and equal */
/*                roots are modified by eps3, */
/*                eps4 is taken very small to avoid overflow :::::::::
: */
	eps2 = norm * .001;
	eps3 = machep * norm;
	uk = (double) (q - p + 1);
	eps4 = uk * eps3;
	uk = eps4 / sqrt(uk);
	s = p;
L505:
	group = 0;
	goto L520;
/*     :::::::::: look for close or coincident roots :::::::::: */
L510:
	if ((d__1 = x1 - x0, abs(d__1)) >= eps2) {
	    goto L505;
	}
	++group;
	if (order * (x1 - x0) <= 0.) {
	    x1 = x0 + order * eps3;
	}
/*     :::::::::: elimination with interchanges and */
/*                initialization of vector :::::::::: */
L520:
	v = 0.;

	i__2 = q;
	for (i = p; i <= i__2; ++i) {
	    rv6[i] = uk;
	    if (i == p) {
		goto L560;
	    }
	    if ((d__1 = e[i], abs(d__1)) < abs(u)) {
		goto L540;
	    }
/*     :::::::::: warning -- a divide check may occur here if */
/*                e2 array has not been specified correctly ::::::
:::: */
	    xu = u / e[i];
	    rv4[i] = xu;
	    rv1[i - 1] = e[i];
	    rv2[i - 1] = d[i] - x1;
	    rv3[i - 1] = 0.;
	    if (i != q) {
		rv3[i - 1] = e[i + 1];
	    }
	    u = v - xu * rv2[i - 1];
	    v = -xu * rv3[i - 1];
	    goto L580;
L540:
	    xu = e[i] / u;
	    rv4[i] = xu;
	    rv1[i - 1] = u;
	    rv2[i - 1] = v;
	    rv3[i - 1] = 0.;
L560:
	    u = d[i] - x1 - xu * v;
	    if (i != q) {
		v = e[i + 1];
	    }
L580:
	    ;
	}

	if (u == 0.) {
	    u = eps3;
	}
	rv1[q] = u;
	rv2[q] = 0.;
	rv3[q] = 0.;
/*     :::::::::: back substitution */
/*                for i=q step -1 until p do -- :::::::::: */
L600:
	i__2 = q;
	for (ii = p; ii <= i__2; ++ii) {
	    i = p + q - ii;
	    rtem = rv6[i] - u * rv2[i] - v * rv3[i];
	    rv6[i] = (rtem) / rv1[i];
	    v = u;
	    u = rv6[i];
/* L620: */
	}
/*     :::::::::: orthogonalize with respect to previous */
/*                members of group :::::::::: */
	if (group == 0) {
	    goto L700;
	}
	j = r;

	i__2 = group;
	for (jj = 1; jj <= i__2; ++jj) {
L630:
	    --j;
	    if (ind[j] != tag) {
		goto L630;
	    }
	    xu = 0.;

	    i__3 = q;
	    for (i = p; i <= i__3; ++i) {
/* L640: */
		xu += rv6[i] * z[i + j * z_dim1];
	    }

	    i__3 = q;
	    for (i = p; i <= i__3; ++i) {
/* L660: */
		rv6[i] -= xu * z[i + j * z_dim1];
	    }

/* L680: */
	}

L700:
	norm = 0.;

	i__2 = q;
	for (i = p; i <= i__2; ++i) {
/* L720: */
	    norm += (d__1 = rv6[i], abs(d__1));
	}

	if (norm >= 1.) {
	    goto L840;
	}
/*     :::::::::: forward substitution :::::::::: */
	if (its == 5) {
	    goto L830;
	}
	if (norm != 0.) {
	    goto L740;
	}
	rv6[s] = eps4;
	++s;
	if (s > q) {
	    s = p;
	}
	goto L780;
L740:
	xu = eps4 / norm;

	i__2 = q;
	for (i = p; i <= i__2; ++i) {
/* L760: */
	    rv6[i] *= xu;
	}
/*     :::::::::: elimination operations on next vector */
/*                iterate :::::::::: */
L780:
	i__2 = q;
	for (i = ip; i <= i__2; ++i) {
	    u = rv6[i];
/*     :::::::::: if rv1(i-1) .eq. e(i), a row interchange */
/*                was performed earlier in the */
/*                triangularization process :::::::::: */
	    if (rv1[i - 1] != e[i]) {
		goto L800;
	    }
	    u = rv6[i - 1];
	    rv6[i - 1] = rv6[i];
L800:
	    rv6[i] = u - rv4[i] * rv6[i - 1];
/* L820: */
	}

	++its;
	goto L600;
/*     :::::::::: set error -- non-converged eigenvector :::::::::: */
L830:
	*ierr = -r;
	xu = 0.;
	goto L870;
/*     :::::::::: normalize so that sum of squares is */
/*                1 and expand to full order :::::::::: */
L840:
	u = 0.;

	i__2 = q;
	for (i = p; i <= i__2; ++i) {
/* L860: */
/* Computing 2nd power */
	    d__1 = rv6[i];
	    u += d__1 * d__1;
	}

	xu = 1. / sqrt(u);

L870:
	i__2 = *n;
	for (i = 1; i <= i__2; ++i) {
/* L880: */
	    z[i + r * z_dim1] = 0.;
	}

	i__2 = q;
	for (i = p; i <= i__2; ++i) {
/* L900: */
	    z[i + r * z_dim1] = rv6[i] * xu;
	}

	x0 = x1;
L920:
	;
    }

    if (q < *n) {
	goto L100;
    }
L1001:
    return 0;
/*     :::::::::: last card of tinvit :::::::::: */
} /* tinvit_ */

/********************************************************/
/********************************************************/
/********************************************************/

/* ./ add name=tridib */

/*     ------------------------------------------------------------------ */

/* Subroutine */ 
/** FUNC DEF */ int jtridib_(int *n, double *eps1, double *d, double *e, double *e2, double *lb, double *ub, int *m11, int *m, double *w, int *ind, int *ierr, 
	double *rv4, double *rv5)
{
    /* Initialized data */

    static double machep = 1.25e-15;

    /* System generated locals */
    int i__1, i__2;
    double d__1, d__2, d__3;

    /* Local variables */
    static int i, j, k, l, p, q, r, s;
    static double u, v;
    static int m1, m2;
    static double t1, t2, x0, x1;
    static int m22, ii;
    static double xu;
    static int isturm, tag;



/*     this subroutine is a translation of the algol procedure bisect, */
/*     num. math. 9, 386-393(1967) by barth, martin, and wilkinson. */
/*     handbook for auto. comp., vol.ii-linear algebra, 249-256(1971). */

/*     this subroutine finds those eigenvalues of a tridiagonal */
/*     symmetric matrix between specified boundary indices, */
/*     using bisection. */

/*     on input: */

/*        n is the order of the matrix; */

/*        eps1 is an absolute error tolerance for the computed */
/*          eigenvalues.  if the input eps1 is non-positive, */
/*          it is reset for each submatrix to a default value, */
/*          namely, minus the product of the relative machine */
/*          precision and the 1-norm of the submatrix; */

/*        d contains the diagonal elements of the input matrix; */

/*        e contains the subdiagonal elements of the input matrix */
/*          in its last n-1 positions.  e(1) is arbitrary; */

/*        e2 contains the squares of the corresponding elements of e. */
/*          e2(1) is arbitrary; */

/*        m11 specifies the lower boundary index for the desired */
/*          eigenvalues; */

/*        m specifies the number of eigenvalues desired.  the upper */
/*          boundary index m22 is then obtained as m22=m11+m-1. */

/*     on output: */

/*        eps1 is unaltered unless it has been reset to its */
/*          (last) default value; */

/*        d and e are unaltered; */

/*        elements of e2, corresponding to elements of e regarded */
/*          as negligible, have been replaced by zero causing the */
/*          matrix to split into a direct sum of submatrices. */
/*          e2(1) is also set to zero; */

/*        lb and ub define an interval containing exactly the desired */
/*          eigenvalues; */

/*        w contains, in its first m positions, the eigenvalues */
/*          between indices m11 and m22 in ascending order; */

/*        ind contains in its first m positions the submatrix indices */
/*          associated with the corresponding eigenvalues in w -- */
/*          1 for eigenvalues belonging to the first submatrix from */
/*          the top, 2 for those belonging to the second submatrix, etc.; 
*/

/*        ierr is set to */
/*          zero       for normal return, */
/*          3*n+1      if multiple eigenvalues at index m11 make */
/*                     unique selection impossible, */
/*          3*n+2      if multiple eigenvalues at index m22 make */
/*                     unique selection impossible; */

/*        rv4 and rv5 are temporary storage arrays. */

/*     note that subroutine tql1, imtql1, or tqlrat is generally faster */
/*     than tridib, if more than n/4 eigenvalues are to be found. */

/*     questions and comments should be directed to b. s. garbow, */
/*     applied mathematics division, argonne national laboratory */

/*     ------------------------------------------------------------------ 
*/

/*     :::::::::: machep is a machine dependent parameter specifying */
/*                the relative precision of floating point arithmetic. */
/*                machep = 16.0d0**(-13) for long form arithmetic */
/*                on s360 :::::::::: */
/*  for f_floating dec fortran */
/*      data machep/1.1d-16/ */
/*  for g_floating dec fortran */
    /* Parameter adjustments */
    --rv5;
    --rv4;
    --e2;
    --e;
    --d;
    --ind;
    --w;

    /* Function Body */

    *ierr = 0;
    tag = 0;
    xu = d[1];
    x0 = d[1];
    u = 0.;
/*     :::::::::: look for small sub-diagonal entries and determine an */
/*                interval containing all the eigenvalues :::::::::: */
    i__1 = *n;
    for (i = 1; i <= i__1; ++i) {
	x1 = u;
	u = 0.;
	if (i != *n) {
	    u = (d__1 = e[i + 1], abs(d__1));
	}
/* Computing MIN */
	d__1 = d[i] - (x1 + u);
	xu = min(d__1,xu);
/* Computing MAX */
	d__1 = d[i] + (x1 + u);
	x0 = max(d__1,x0);
	if (i == 1) {
	    goto L20;
	}
	if ((d__1 = e[i], abs(d__1)) > machep * ((d__2 = d[i], abs(d__2)) + (
		d__3 = d[i - 1], abs(d__3)))) {
	    goto L40;
	}
L20:
	e2[i] = 0.;
L40:
	;
    }

/* Computing MAX */
    d__1 = abs(xu), d__2 = abs(x0);
    x1 = max(d__1,d__2) * machep * (double) (*n);
    xu -= x1;
    t1 = xu;
    x0 += x1;
    t2 = x0;
/*     :::::::::: determine an interval containing exactly */
/*                the desired eigenvalues :::::::::: */
    p = 1;
    q = *n;
    m1 = *m11 - 1;
    if (m1 == 0) {
	goto L75;
    }
    isturm = 1;
L50:
    v = x1;
    x1 = xu + (x0 - xu) * .5;
    if (x1 == v) {
	goto L980;
    }
    goto L320;
L60:
    if ((i__1 = s - m1) < 0) {
	goto L65;
    } else if (i__1 == 0) {
	goto L73;
    } else {
	goto L70;
    }
L65:
    xu = x1;
    goto L50;
L70:
    x0 = x1;
    goto L50;
L73:
    xu = x1;
    t1 = x1;
L75:
    m22 = m1 + *m;
    if (m22 == *n) {
	goto L90;
    }
    x0 = t2;
    isturm = 2;
    goto L50;
L80:
    if ((i__1 = s - m22) < 0) {
	goto L65;
    } else if (i__1 == 0) {
	goto L85;
    } else {
	goto L70;
    }
L85:
    t2 = x1;
L90:
    q = 0;
    r = 0;
/*     :::::::::: establish and process next submatrix, refining */
/*                interval by the gerschgorin bounds :::::::::: */
L100:
    if (r == *m) {
	goto L1001;
    }
    ++tag;
    p = q + 1;
    xu = d[p];
    x0 = d[p];
    u = 0.;

    i__1 = *n;
    for (q = p; q <= i__1; ++q) {
	x1 = u;
	u = 0.;
	v = 0.;
	if (q == *n) {
	    goto L110;
	}
	u = (d__1 = e[q + 1], abs(d__1));
	v = e2[q + 1];
L110:
/* Computing MIN */
	d__1 = d[q] - (x1 + u);
	xu = min(d__1,xu);
/* Computing MAX */
	d__1 = d[q] + (x1 + u);
	x0 = max(d__1,x0);
	if (v == 0.) {
	    goto L140;
	}
/* L120: */
    }

L140:
/* Computing MAX */
    d__1 = abs(xu), d__2 = abs(x0);
    x1 = max(d__1,d__2) * machep;
    if (*eps1 <= 0.) {
	*eps1 = -x1;
    }
    if (p != q) {
	goto L180;
    }
/*     :::::::::: check for isolated root within interval :::::::::: */
    if (t1 > d[p] || d[p] >= t2) {
	goto L940;
    }
    m1 = p;
    m2 = p;
    rv5[p] = d[p];
    goto L900;
L180:
    x1 *= (double) (q - p + 1);
/* Computing MAX */
    d__1 = t1, d__2 = xu - x1;
    *lb = max(d__1,d__2);
/* Computing MIN */
    d__1 = t2, d__2 = x0 + x1;
    *ub = min(d__1,d__2);
    x1 = *lb;
    isturm = 3;
    goto L320;
L200:
    m1 = s + 1;
    x1 = *ub;
    isturm = 4;
    goto L320;
L220:
    m2 = s;
    if (m1 > m2) {
	goto L940;
    }
/*     :::::::::: find roots by bisection :::::::::: */
    x0 = *ub;
    isturm = 5;

    i__1 = m2;
    for (i = m1; i <= i__1; ++i) {
	rv5[i] = *ub;
	rv4[i] = *lb;
/* L240: */
    }
/*     :::::::::: loop for k-th eigenvalue */
/*                for k=m2 step -1 until m1 do -- */
/*                (-do- not used to legalize -computed go to-) :::::::::: 
*/
    k = m2;
L250:
    xu = *lb;
/*     :::::::::: for i=k step -1 until m1 do -- :::::::::: */
    i__1 = k;
    for (ii = m1; ii <= i__1; ++ii) {
	i = m1 + k - ii;
	if (xu >= rv4[i]) {
	    goto L260;
	}
	xu = rv4[i];
	goto L280;
L260:
	;
    }

L280:
    if (x0 > rv5[k]) {
	x0 = rv5[k];
    }
/*     :::::::::: next bisection step :::::::::: */
L300:
    x1 = (xu + x0) * .5;
    if (x0 - xu <= machep * 2. * (abs(xu) + abs(x0)) + abs(*eps1)) {
	goto L420;
    }
/*     :::::::::: in-line procedure for sturm sequence :::::::::: */
L320:
    s = p - 1;
    u = 1.;

    i__1 = q;
    for (i = p; i <= i__1; ++i) {
	if (u != 0.) {
	    goto L325;
	}
	v = (d__1 = e[i], abs(d__1)) / machep;
	if (e2[i] == 0.) {
	    v = 0.;
	}
	goto L330;
L325:
	v = e2[i] / u;
L330:
	u = d[i] - x1 - v;
	if (u < 0.) {
	    ++s;
	}
/* L340: */
    }

    switch ((int)isturm) {
	case 1:  goto L60;
	case 2:  goto L80;
	case 3:  goto L200;
	case 4:  goto L220;
	case 5:  goto L360;
    }
/*     :::::::::: refine intervals :::::::::: */
L360:
    if (s >= k) {
	goto L400;
    }
    xu = x1;
    if (s >= m1) {
	goto L380;
    }
    rv4[m1] = x1;
    goto L300;
L380:
    rv4[s + 1] = x1;
    if (rv5[s] > x1) {
	rv5[s] = x1;
    }
    goto L300;
L400:
    x0 = x1;
    goto L300;
/*     :::::::::: k-th eigenvalue found :::::::::: */
L420:
    rv5[k] = x1;
    --k;
    if (k >= m1) {
	goto L250;
    }
/*     :::::::::: order eigenvalues tagged with their */
/*                submatrix associations :::::::::: */
L900:
    s = r;
    r = r + m2 - m1 + 1;
    j = 1;
    k = m1;

    i__1 = r;
    for (l = 1; l <= i__1; ++l) {
	if (j > s) {
	    goto L910;
	}
	if (k > m2) {
	    goto L940;
	}
	if (rv5[k] >= w[l]) {
	    goto L915;
	}

	i__2 = s;
	for (ii = j; ii <= i__2; ++ii) {
	    i = l + s - ii;
	    w[i + 1] = w[i];
	    ind[i + 1] = ind[i];
/* L905: */
	}

L910:
	w[l] = rv5[k];
	ind[l] = tag;
	++k;
	goto L920;
L915:
	++j;
L920:
	;
    }

L940:
    if (q < *n) {
	goto L100;
    }
    goto L1001;
/*     :::::::::: set error -- interval cannot be found containing */
/*                exactly the desired eigenvalues :::::::::: */
L980:
    *ierr = *n * 3 + isturm;
L1001:
    *lb = t1;
    *ub = t2;
    return 0;
/*     :::::::::: last card of tridib :::::::::: */
} /* tridib_ */

/*************************************************************/
/*************************************************************/
/*************************************************************/




/** FUNC DEF */ float *fjector(long nl, long nh)
/* allocate a float jector with subscript range v[nl..nh] */
{
	float *v;
        
	v=(float *)R_alloc((size_t) (nh-nl+1+NR_END),sizeof(float));
	
	if (!v) REprintf("allocation failure in jector()\n");
	return v-nl+NR_END;
}



/** FUNC DEF */ double  *djector(long nl, long nh)
/* allocate a double jector with subscript range v[nl..nh] */
{
	double  *v;
         
	v=(double  *)R_alloc((size_t) (nh-nl+1+NR_END),sizeof(double));
	
	if (!v) REprintf("allocation failure in djector()\n");
	return v-nl+NR_END;
}
/** FUNC DEF */ int  *ijector(long nl, long nh)
/* allocate an int  jector with subscript range v[nl..nh] */
{
	int  *v;
       
	v=(int *)R_alloc((size_t) (nh-nl+1+NR_END),sizeof(int));
	
	if (!v) REprintf("allocation failure in jijector()\n");
	return v-nl+NR_END;
}


/** FUNC DEF */ void free_fjector(float *v, long nl, long nh)
/* free a float jector allocated with jector() */
{
	free((FREE_ARG) (v+nl-NR_END));
}

/** FUNC DEF */ void free_ijector(int *v, long nl, long nh)
/* free an int jector allocated with ijector() */
{
	free((FREE_ARG) (v+nl-NR_END));
}
/** FUNC DEF */ void free_djector(double *v, long nl, long nh)
/* free a double jector allocated with djector() */
{
	free((FREE_ARG) (v+nl-NR_END));
}

/*************************************************************/
/*************************************************************/
/*************************************************************/


void 
blank(void)
{
	REprintf( "\n");
}

/** FUNC DEF */ int  multitap(int num_points, int nwin, double *lam, double npi, double *tapers, double *tapsum)
{
	/*
	 * get the multitaper slepian functions: 
	num_points = number of points in data stream
	nwin = number of windows 
	lam= jector of eigenvalues 
	npi = order of
	slepian functions 
	tapsum = sum of each taper, saved for use in adaptive weighting  
	tapers =  matrix of slepian tapers, packed in a 1D double array
	 */

	int             i,  k, kk;
	double          ww, cs, ai, an, eps, rlu, rlb, aa;
	double          dfac, drat, gamma, bh, tapsq, TWOPI, DPI;
	double         *diag, *offdiag, *offsq;
	/* char           *k1[4]; */

	/* char            name[81]; */
	double         *scratch1, *scratch2, *scratch3, *scratch4, *scratch6;


	/* need to initialize iwflag = 0 */
	double          anpi;
	double         *ell;
	/* int             key, nbin, npad; */
	int            *ip;
	double         *evecs;
/* 	double         *zee; */

	long            len;
	int             ierr;
	int             m11;
	DPI = (double) JMLPI;
	TWOPI = (double) 2 *DPI;

	anpi = npi;
	an = (double) (num_points);
	ww = (double) (anpi) / an;	/* this corresponds to P&W's W value  */
	cs = cos(TWOPI * ww);

	ell = djector((long) 0, (long) nwin);

	diag = djector((long) 0, (long) num_points);
	offdiag = djector((long) 0, (long) num_points);
	offsq = djector((long) 0, (long) num_points);

	scratch1 = djector((long) 0, (long) num_points);
	scratch2 = djector((long) 0, (long) num_points);
	scratch3 = djector((long) 0, (long) num_points);
	scratch4 = djector((long) 0, (long) num_points);
	scratch6 = djector((long) 0, (long) num_points);



	/* make the diagonal elements of the tridiag matrix  */

	for (i = 0; i < num_points; i++) {
		ai = (double) (i);
		diag[i] = -cs * (((an - 1.) / 2. - ai)) * (((an - 1.) / 2. - ai));
		offdiag[i] = -ai * (an - ai) / 2.;
		offsq[i] = offdiag[i] * offdiag[i];
	}

	eps = 1.0e-13;
	m11 = 1;


	ip = ijector((long) 0, (long) nwin);

	/* call the eispac routines to invert the tridiagonal system */

	jtridib_(&num_points, &eps, diag, offdiag, offsq, &rlb, &rlu, &m11, &nwin, lam,
		 ip, &ierr, scratch1, scratch2);
#if DIAG1
	REprintf( "ierr=%d rlb=%.8f rlu=%.8f\n", ierr, rlb, rlu);

	REprintf( "eigenvalues for the eigentapers\n");

	for (k = 0; k < nwin; k++)
		REprintf("%.20f ", lam[k]);
	blank();
#endif


	len = num_points * nwin;
	evecs = djector((long) 0, (long) len);


	jtinvit_(&num_points, &num_points, diag, offdiag, offsq, &nwin, lam, ip, evecs, &ierr,
		 scratch1, scratch2, scratch3, scratch4, scratch6);



	/*
	free_djector(scratch1, (long) 0, (long) num_points);
	free_djector(scratch2, (long) 0, (long) num_points);
	free_djector(scratch3, (long) 0, (long) num_points);
	free_djector(scratch4, (long) 0, (long) num_points);
	free_djector(scratch6, (long) 0, (long) num_points);
	*/


	/*
	 * we calculate the eigenvalues of the dirichlet-kernel problem i.e.
	 * the bandwidth retention factors from slepian 1978 asymptotic
	 * formula, gotten from thomson 1982 eq 2.5 supplemented by the
	 * asymptotic formula for k near 2n from slepian 1978 eq 61 more
	 * precise values of these parameters, perhaps useful in adaptive
	 * spectral estimation, can be calculated explicitly using the
	 * rayleigh-quotient formulas in thomson (1982) and park et al (1987)
	 * 
	 */
	dfac = (double) an *DPI * ww;
	drat = (double) 8. *dfac;


	dfac = (double) 4. *sqrt(DPI * dfac) * exp((double) (-2.0) * dfac);


	for (k = 0; k < nwin; k++) {
		lam[k] = (double) 1.0 - (double) dfac;
		dfac = dfac * drat / (double) (k + 1);



		/* fails as k -> 2n */
	}


	gamma = log((double) 8. * an * sin((double) 2. * DPI * ww)) + (double) 0.5772156649;



	for (k = 0; k < nwin; k++) {
		bh = -2. * DPI * (an * ww - (double) (k) /
				  (double) 2. - (double) .25) / gamma;
		ell[k] = (double) 1. / ((double) 1. + exp(DPI * (double) bh));

	}

	for (i = 0; i < nwin; i++)
		lam[i] = MAX(ell[i], lam[i]);

	/************************************************************
        c   normalize the eigentapers to preserve power for a white process
        c   i.e. they have rms value unity
        c  tapsum is the average of the eigentaper, should be near zero for
        c  antisymmetric tapers
        ************************************************************/

	for (k = 0; k < nwin; k++) {
		kk = (k) * num_points;
		tapsum[k] = 0.;
		tapsq = 0.;
		for (i = 0; i < num_points; i++) {
			aa = evecs[i + kk];
			tapers[i + kk] = aa;
			tapsum[k] = tapsum[k] + aa;
			tapsq = tapsq + aa * aa;
		}
		aa = sqrt(tapsq / (double) num_points);
		tapsum[k] = tapsum[k] / aa;

		for (i = 0; i < num_points; i++) {
			tapers[i + kk] = tapers[i + kk] / aa;

		}
	}


	/* Free Memory */

	/*
	free_djector(ell, (long) 0, (long) nwin);
	free_djector(diag, (long) 0, (long) num_points);
	free_djector(offdiag, (long) 0, (long) num_points);
	free_djector(offsq, (long) 0, (long) num_points);
	free_ijector(ip, (long) 0, (long) nwin);

	free_djector(evecs, (long) 0, (long) len);
	*/

	return 1;
}

/*************************************************************/
/*************************************************************/
/*************************************************************/






/*************************************************************/
/*************************************************************/
/*************************************************************/


/** FUNC DEF */ void jrealft(double data[], unsigned long n, int isign)
{

	unsigned long i,i1,i2,i3,i4,np3;
	double  c1=0.5,c2,h1r,h1i,h2r,h2i;
	double wr,wi,wpr,wpi,wtemp,theta;

	theta=3.141592653589793/(double) (n>>1);
	if (isign == 1) {
		c2 = -0.5;
		dfour1(data,n>>1,1);
	} else {
		c2=0.5;
		theta = -theta;
	}
	wtemp=sin(0.5*theta);
	wpr = -2.0*wtemp*wtemp;
	wpi=sin(theta);
	wr=1.0+wpr;
	wi=wpi;
	np3=n+3;
	for (i=2;i<=(n>>2);i++) {
		i4=1+(i3=np3-(i2=1+(i1=i+i-1)));
		h1r=c1*(data[i1]+data[i3]);
		h1i=c1*(data[i2]-data[i4]);
		h2r = -c2*(data[i2]+data[i4]);
		h2i=c2*(data[i1]-data[i3]);
		data[i1]=h1r+wr*h2r-wi*h2i;
		data[i2]=h1i+wr*h2i+wi*h2r;
		data[i3]=h1r-wr*h2r+wi*h2i;
		data[i4] = -h1i+wr*h2i+wi*h2r;
		wr=(wtemp=wr)*wpr-wi*wpi+wr;
		wi=wi*wpr+wtemp*wpi+wi;
	}
	if (isign == 1) {
		data[1] = (h1r=data[1])+data[2];
		data[2] = h1r-data[2];
	} else {
		data[1]=c1*((h1r=data[1])+data[2]);
		data[2]=c1*(h1r-data[2]);
		dfour1(data,n>>1,-1);
	}
}


/** FUNC DEF */void dfour1(double data[], unsigned long nn, int isign)
{
	unsigned long n,mmax,m,j,istep,i;
	double wtemp,wr,wpr,wpi,wi,theta;
	double  tempr,tempi;

	n=nn << 1;
	j=1;
	for (i=1;i<n;i+=2) {
		if (j > i) {
			SWAP(data[j],data[i]);
			SWAP(data[j+1],data[i+1]);
		}
		m=n >> 1;
		while (m >= 2 && j > m) {
			j -= m;
			m >>= 1;
		}
		j += m;
	}
	mmax=2;
	while (n > mmax) {
		istep=mmax << 1;
		theta=isign*(6.28318530717959/mmax);
		wtemp=sin(0.5*theta);
		wpr = -2.0*wtemp*wtemp;
		wpi=sin(theta);
		wr=1.0;
		wi=0.0;
		for (m=1;m<mmax;m+=2) {
			for (i=m;i<=n;i+=istep) {
				j=i+mmax;
				tempr=wr*data[j]-wi*data[j+1];
				tempi=wr*data[j+1]+wi*data[j];
				data[j]=data[i]-tempr;
				data[j+1]=data[i+1]-tempi;
				data[i] += tempr;
				data[i+1] += tempi;
			}
			wr=(wtemp=wr)*wpr-wi*wpi+wr;
			wi=wi*wpr+wtemp*wpi+wi;
		}
		mmax=istep;
	}
}
/*************************************************************/
/*************************************************************/
/*************************************************************/


/** FUNC DEF */ int adwait(double *sqr_spec,  double *dcf,
            double *el, int nwin, int num_freq, double *ares, double *degf, double avar)
{
/*
c  this version uses thomson's algorithm for calculating 
c  the adaptive spectrum estimate
*/
      double as,das,tol,a1,scale,ax,fn,fx;
      double *spw, *bias;
      double test_tol, dif;
      int jitter, i,k, kpoint, jloop;
       double df;
       /*
c  set tolerance for iterative scheme exit */


#if 0
  REprintf("test input\n adwait: %d %d %f\n",nwin, num_freq, avar);
       REprintf("\n Data=\n");
    for( i =0; i<num_freq; i++)
       {
            REprintf("%d %f \n",i,sqr_spec[i]);
          }
#endif


      tol=3.0e-4;
      jitter=0;
      scale=avar;
                 /***********************************
                 c  we scale the bias by the total variance of the frequency transform
                 c  from zero freq to the nyquist
                 c  in this application we scale the eigenspectra by the bias in order to avoid
                 c  possible floating point overflow
                 ************************************/
      spw=djector(0,nwin);
      bias=djector(0,nwin);
      for( i=0;i<nwin; i++)
          {
            
            bias[i]=(1.00-el[i]);
             }

    

       /* START do 100 */
    for( jloop=0; jloop<num_freq; jloop++)
    {   
        
       for( i=0;i<nwin; i++)
         {  kpoint=jloop+i*num_freq;
            spw[i]=(sqr_spec[kpoint])/scale ;
           }
                          /********************************************
                          c  first guess is the average of the two 
                              lowest-order eigenspectral estimates
                           ********************************************/
       as=(spw[0]+spw[1])/2.00;

                              /* START do 300 */
                              /* c  find coefficients */

        for( k=0; k<20 ; k++) 
        {
          fn=0.00;
          fx=0.00;

          for( i=0;i<nwin; i++)
           {
               a1=sqrt(el[i])*as/(el[i]*as+bias[i]);
               a1=a1*a1;
               fn=fn+a1*spw[i];
               fx=fx+a1;
           }
  

         ax=fn/fx;
         dif = ax-as;
         das=ABS(dif);
      
         test_tol = das/as;
         if( test_tol < tol )
            { 
                   break;
               }

         as=ax;
        }

        
                            /* end  300  */

                           /* c  flag if iteration does not converge */

      if(k>=20)  jitter++;
  
       ares[jloop]=as*scale;
                            /* c   calculate degrees of freedom */
      df=0.0;
      for( i=0;i< nwin; i++)
       {
          kpoint=jloop+i*num_freq;
          dcf[kpoint]=sqrt(el[i])*as/(el[i]*as+bias[i]);
          df=df+dcf[kpoint]*dcf[kpoint];
       }
 			/*
			 * we normalize degrees of freedom by the weight of
			 * the first eigenspectrum this way we never have
			 * fewer than two degrees of freedom
			 */

       degf[jloop]=df*2./(dcf[jloop]*dcf[jloop]);

  }                                       /* end 100 */

   
    /*
      free_djector(spw,0,nwin);
      free_djector(bias,0,nwin);
    */

     return jitter;
}

/*************************************************************/
/*************************************************************/
/*************************************************************/

/** FUNC DEF */ void  get_F_values(double *sr, double *si, int nf, int nwin, double *Fvalue, double *b)
{
	/*
	 * b is fft of slepian eigentapers at zero freq sr si are the
	 * eigenspectra amu contains line frequency estimates and f-test
	 * parameter
	 */
	double          sum, sumr, sumi, sum2;
	int             i, j, k;
	double         *amur, *amui;
	sum = 0.;
	amur = djector((long) 0, (long) nf);
	amui = djector((long) 0, (long) nf);



	for (i = 0; i < nwin; i++) {
                
		sum = sum + b[i] * b[i];
	}
	for (i = 0; i < nf; i++) {
		amur[i] = 0.;
		amui[i] = 0.;
		for (j = 0; j < nwin; j++) {
			k = i + j * nf;
			amur[i] = amur[i] + sr[k] * b[j];
			amui[i] = amui[i] + si[k] * b[j];
		}
		amur[i] = amur[i] / sum;
		amui[i] = amui[i] / sum;
		sum2 = 0.;
		for (j = 0; j < nwin; j++) {
			k = i + j * nf;
			sumr = sr[k] - amur[i] * b[j];
			sumi = si[k] - amui[i] * b[j];
			sum2 = sum2 + sumr * sumr + sumi * sumi;
		}
		Fvalue[i] = (double) (nwin - 1) * (SQR(amui[i]) + SQR(amur[i])) * sum / sum2;
                /* percival and walden, eq 499c, p499 */
              /* sum = Hk(0) squared  */
	}
	return;
}

/*************************************************************/
/*************************************************************/
/*************************************************************/
/*-------------  HIRES  ----------------------------------*/
/*-------------------------------------------------------*/
/** FUNC DEF */ int hires(double *sqr_spec,  double *el, int nwin, int num_freq, double *ares)
{
	int             i, j, k, kpoint;
	double           a;

	for (j = 0; j < num_freq; j++)
		ares[j] = 0.;

	for (i = 0; i < nwin; i++) {
		k = i * num_freq;
		a = 1. / (el[i] * nwin);
		for (j = 0; j < num_freq; j++) {
			kpoint = j + k;
			ares[j] = ares[j] +
				a * ( sqr_spec[kpoint] );
		}
	}

	for (j = 0; j < num_freq; j++) {
		if(ares[j]>0.0) 
                   ares[j] = sqrt(ares[j]);
                  else Rprintf("sqrt problem in hires pos=%d %f\n", j, ares[j]);
	}

	return 1;
}      
/*************************************************************/
/*************************************************************/
/*************************************************************/




/*------------------------------------------------------------*/
/*------------------------------------------------------------*/
/** FUNC DEF */ void zero_pad(double output[], int start , int olength)
{
    int i;
    for( i= start ; i< olength; i++) 
         {   
            output[i] = 0.0; 
               }
}


/*------------------------------------------------------------*/
/*------------------------------------------------------------*/

/** FUNC DEF */ void  mt_get_spec(double *series, int inum, int klength, double *amp)
{
/*    series = input time series
      inum   = length of time series
      klength = number of elements in power spectrum (a power of 2)
      amp = returned power spectrum
*/

	int             i,  isign = 1;

	unsigned long   nn;
	/* double           tsv; */


    	nn = klength;

        

	/* copy amp onto series and apply zero padding to  klength */
       
	for (i = 0; i < inum; i++) {
		
		amp[i] = series[i];
 	
	}

       zero_pad(amp, inum, klength);


/*  Fast Fourier Transform Routine:  here we are using the Numerical Recipes
     routine jrealft which returns the fft in the 1-D input array
     packed as pairs of real numbers.
     The jrealft routine requires the input array to start at index=1
      so we must decrement the index of amp
*/


 
          jrealft(amp-1, nn, isign);


}

/** FUNC DEF */ void  do_mtap_spec(double *data, int npoints, int kind,
	    int nwin, double npi, int inorm, double dt, double *ospec, 
				   double *dof, double *Fvalues, int klen)
{
/*
     data = doubleing point input time series
    npoints = number of points in data
     kind = flag for choosing hires or adaptive weighting coefficients
     nwin = number of taper windows to calculate
     npi = order of the slepian functions
     inorm = flag for choice of normalization
     dt = sampling interval (time)
     ospec = output spctrum
    dof = degrees of freedom at each frequency
    Fvalues = Ftest value at each frequency estimate
    klen = number of frequecies calculated (power of 2)

*/

	int             i, j, k;
	double         *lambda, *tapers;
/* 	long            len, longlen; */
	/* double          *xt; */
	FILE           *fopen(const char *filename, const char *mode);
/* , *inf; */
	/* , *tap_file */;
       /*  FILE            *dof_file; */

	/* int             logg; */
	/* int             nn; */
	double          *b;
	int             iwin, kk;

	/*************/
	double          anrm, norm;
        double            *ReSpec, *ImSpec;
	double         *sqr_spec,  *amu;
	double          *amp, *fv;
	double          avamp, temp, sqramp;
	double          sum, *tapsum;
	/************/
         int num_freqs;
         int len_taps, num_freq_tap;
   
	double         *dcf, *degf, avar;
	int             n1, n2, kf;
	/* int             flag; */
	/* int             one = 1; */

         /* double tem1, tem2; */

/* lambda = vector of eigenvalues   
   tapsum = sum of each taper, saved for use in adaptive weighting  
   tapers =  matrix of slepian tapers, packed in a 1D double array    
*/



             

	lambda = djector((long)0, (long)nwin);
        tapsum=djector((long)0,(long)nwin);
	len_taps = npoints * nwin;
	tapers = djector((long)0,(long) len_taps);

             num_freqs = 1+klen/2;
             num_freq_tap = num_freqs*nwin;



       /* get a slepian taper  */

	k = multitap(npoints, nwin, lambda,  npi, tapers, tapsum);



	
     /* choose normalization based on inorm flag  */

	anrm = 1.;

	switch (inorm) {
	case 0:
		anrm = 1.;
		break;

	case 1:
		anrm = npoints;
		break;
	case 2:
		anrm = 1 / dt;
		break;
	case 3:
		anrm = sqrt((double) npoints);
		break;
	default:
		anrm = 1.;
		break;
	}

	
	/* apply the taper in the loop.  do this nwin times  */

	b = djector((long)0, (long)npoints);
	amu = djector((long)0,(long) num_freqs);
	sqr_spec = djector((long)0,(long) num_freq_tap);
        ReSpec = djector((long)0,(long) num_freq_tap);
        ImSpec = djector((long)0,(long) num_freq_tap);


	for (iwin = 0; iwin < nwin; iwin++) {
		kk = iwin * npoints;
                kf = iwin * num_freqs;

		for (j = 0; j < npoints; j++)
			b[j] = data[j] * tapers[kk + j];   /*  application of  iwin-th taper   */
		
		amp = djector((long)0,(long) klen);

		mt_get_spec(b, npoints, klen, amp);  /* calculate the eigenspectrum */

	       
          
		
		sum = 0.0;


/* get spectrum from real fourier transform    */


          norm = 1.0/(anrm*anrm);

     



            for(i=1; i<num_freqs-1; i++){
       if(2*i+1 > klen) REprintf("error in index\n");
       if(i+kf > num_freq_tap ) REprintf("error in index\n");

            sqramp = SQR(amp[2*i+1])+SQR(amp[2*i]);

            ReSpec[i+kf] = amp[2*i];
            ImSpec[i+kf] = amp[2*i+1];



            sqr_spec[i+kf] =    norm*(sqramp);

             sum += sqr_spec[i+kf];
            }
          sqr_spec[0+kf] = norm*SQR(fabs(amp[0]));
          sqr_spec[num_freqs-1+kf] = norm*SQR(fabs(amp[1]));

            ReSpec[0+kf] = amp[0];
            ImSpec[0+kf] = 0.0;

            ReSpec[num_freqs-1+kf] = amp[1];
            ImSpec[num_freqs-1+kf] = 0.0;

             sum += sqr_spec[0+kf] + sqr_spec[num_freqs-1+kf];

        if(num_freqs-1+kf>num_freq_tap )REprintf("error in index\n");

		temp = sum / (double) num_freqs;
		if (temp > 0.0)
			avamp = sqrt(temp) / anrm;
		else {
			avamp = 0.0;
			 
		}


		/* free_djector(amp,(long) 0,(long) klen); */

	}

                 /* free_djector(b, (long)0, (long)npoints); */
 		fv = djector((long)0,(long) num_freqs); 

        /* choice of hi-res or adaptive weighting for spectra    */


#if 0
		if ((inf = fopen("mspec.file", "w")) == NULL) {
			REprintf("mspec.file unable to open\n");
			return;
		}
	
		for (i = 0; i < num_freqs; i++) {
	for (iwin = 0; iwin < nwin; iwin++) {
	
		kf = iwin * num_freqs;

			Rprintf(inf, "%f %f ", ReSpec[i + kf], ImSpec[i + kf]);
		}
             	Rprintf(inf, "\n");
	}
	
	fclose(inf);

#endif 



	switch (kind) {
	case 1:
	
		hires(sqr_spec,  lambda, nwin, num_freqs, amu);
	        get_F_values(ReSpec, ImSpec, num_freqs, nwin, fv, tapsum);
 

 
           for (i = 0; i < num_freqs; i++) {
		ospec[i] =amu[i];
                 dof[i] = nwin-1;
                 Fvalues[i] = fv[i];
                  }

		break;

	case 2:


		/* get avar = variance*/

		n1 = 0;
		n2 = npoints;


                  avar = 0.0;

		for (i = n1; i < n2; i++)
			avar += (data[i]) * (data[i]);


		switch (inorm) {
		case 0:
			avar = avar / npoints;
			break;

		case 1:
			avar = avar / (npoints * npoints);
			break;

		case 2:
			avar = avar * dt * dt;
			break;

		case 3:

			avar = avar / npoints;
			break;

		default:
			break;
		}

		 


		dcf = djector((long)0,(long) num_freq_tap);
		degf = djector((long)0,(long) num_freqs);

	


		adwait(sqr_spec, dcf, lambda, nwin, num_freqs, amu, degf, avar);

                get_F_values(ReSpec, ImSpec, num_freqs, nwin, fv, tapsum);

#if 0
           /* dump out the degrees of freedom to a file for later inspection  */
              	if ((dof_file = fopen("dof_file", "w")) == NULL) {
		REprintf("dof unable to open\n");
		return;}
               	for (i = 0; i < num_freqs; i++) {
	                Rprintf(dof_file,"%f\n",degf[i]);
                       	}

                   fclose(dof_file);
#endif

                 /* rap up   */

           for (i = 0; i < num_freqs; i++) {
		ospec[i] =amu[i];
                 dof[i] = degf[i];
                 Fvalues[i] = fv[i];
                  }


                
	/* 	free_djector(dcf,(long)0,(long) num_freq_tap); */
/* 		free_djector(degf,(long)0,(long) num_freqs); */
/* 		free_djector(fv,(long)0,(long) num_freqs); */


		break;
	}

/*  free up memory and return  */

        /* free_djector(amu,(long)0,(long) num_freqs); */
        

/* 	free_djector(sqr_spec, (long)0,(long) num_freq_tap); */
/* 	free_djector(ReSpec, (long)0,(long) num_freq_tap); */

/* 	free_djector(ImSpec, (long)0,(long) num_freq_tap); */

/* 	free_djector(lambda,(long) 0,(long) nwin); */

/* 	free_djector(tapers,(long) 0, (long)len_taps); */
/* 	free_djector(tapsum,(long) 0, (long)nwin); */

}


/*************************************************************/
/*************************************************************/
/*************************************************************/

/** FUNC DEF */ void  Mtap_spec(double *data, int npoints, int kind,
	    int nwin, double npi, int inorm, double dt, double *ospec, double *dof, 
		double *Fvalues, int klen, double  *ReSpec, double   *ImSpec)
{
/*
     data = floating point input time series
    npoints = number of points in data
     kind = flag for choosing hires or adaptive weighting coefficients
     nwin = number of taper windows to calculate
     npi = order of the slepian functions
     inorm = flag for choice of normalization
     dt = sampling interval (time)
     ospec = output spctrum
    dof = degrees of freedom at each frequency
    Fvalues = Ftest value at each frequency estimate
    klen = number of frequecies calculated (power of 2)

*/

	int             i, j, k;
	double         *lambda, *tapers;
/* 	long            len, longlen; */
/* 	double          *xt; */
	FILE           *fopen(const char *filename, const char *mode);
/* , *inf, *tap_file; */
        /* FILE            *dof_file; */

/* 	int             logg; */
/* 	int             nn; */
	double          *b;
	int             iwin, kk;

	/*************/
	double          anrm, norm;
        
	double         *sqr_spec,  *amu;
	double          *amp, *fv;
	double          avamp, temp, sqramp;
	double          sum, *tapsum;
	/************/
         int num_freqs;
         int len_taps, num_freq_tap;
   
	double         *dcf, *degf, avar;
	int             n1, n2, kf;
	/* int             flag; */
	/* int             one = 1; */

         /* double tem1, tem2; */

/* lambda = vector of eigenvalues   
   tapsum = sum of each taper, saved for use in adaptive weighting  
   tapers =  matrix of slepian tapers, packed in a 1D double array    
*/



	lambda = djector((long)0, (long)nwin);
        tapsum=  djector((long)0,(long)nwin);
	len_taps = npoints * nwin;
	tapers = djector((long)0,(long) len_taps);

             num_freqs = 1+klen/2;
             num_freq_tap = num_freqs*nwin;



       /* get a slepian taper  */

	k = multitap(npoints, nwin, lambda,  npi, tapers, tapsum);

#if 0
	for (j = 0; j < npoints; j++)
	{
	   for (iwin = 0; iwin < nwin; iwin++) 
	   {
	      kk = iwin * npoints;
	   
	      
	      Rprintf(stdout, "%f ", tapers[kk + j]);
 
	   }
	   Rprintf(stdout, "\n");
	}
	
	
	
#endif 

	/* Rprintf(stderr, "DONE: multitap\n"); */
     /* choose normalization based on inorm flag  */

	anrm = 1.;

	switch (inorm) {
	case 0:
		anrm = 1.;
		break;

	case 1:
		anrm = npoints;
		break;
	case 2:
		anrm = 1 / dt;
		break;
	case 3:
		anrm = sqrt((double) npoints);
		break;
	default:
		anrm = 1.;
		break;
	}

	
	/* apply the taper in the loop.  do this nwin times  */

	b = djector((long)0, (long)npoints);
	amu = djector((long)0,(long) num_freqs);
	sqr_spec = djector((long)0,(long) num_freq_tap);


	for (iwin = 0; iwin < nwin; iwin++) 
	{
	   kk = iwin * npoints;
	   kf = iwin * num_freqs;
	   
	   for (j = 0; j < npoints; j++)
	      b[j] = data[j] * tapers[kk + j];   /*  application of  iwin-th taper   */
	   
	   amp = djector((long)0,(long) klen);
	  

	   mt_get_spec(b, npoints, klen, amp);  /* calculate the eigenspectrum */
	   
	   
	  
	   
	   sum = 0.0;
	   
	   
/* get spectrum from real fourier transform    */

	   
	   norm = 1.0/(anrm*anrm);

     



	   for(i=1; i<num_freqs-1; i++)
	   {
	      if(2*i+1 > klen) REprintf("error in index\n");
	      if(i+kf > num_freq_tap ) REprintf("error in index\n");
	      
	      sqramp = SQR(amp[2*i+1])+SQR(amp[2*i]);
	      
	      ReSpec[i+kf] = amp[2*i];
	      ImSpec[i+kf] = amp[2*i+1];
	      
	      
	      
	      sqr_spec[i+kf] =    norm*(sqramp);
	      
	      sum += sqr_spec[i+kf];
	   }
	   sqr_spec[0+kf] = norm*SQR(fabs(amp[0]));
	   sqr_spec[num_freqs-1+kf] = norm*SQR(fabs(amp[1]));
	   
	   ReSpec[0+kf] = amp[0];
	   ImSpec[0+kf] = 0.0;
	   
	   ReSpec[num_freqs-1+kf] = amp[1];
	   ImSpec[num_freqs-1+kf] = 0.0;

	   sum += sqr_spec[0+kf] + sqr_spec[num_freqs-1+kf];
	   
	   if(num_freqs-1+kf>num_freq_tap )REprintf("error in index\n");
	   
	   temp = sum / (double) num_freqs;
	   if (temp > 0.0)
	      avamp = sqrt(temp) / anrm;
	   else {
	      avamp = 0.0;
	      
	   }
	   
	   
	   /* free_djector(amp,(long) 0,(long) klen); */
	   
	}
	
/* 	free_djector(b, (long)0, (long)npoints); */
	fv = djector((long)0,(long) num_freqs);
	
        /* choice of hi-res or adaptive weighting for spectra    */





	switch (kind) {
	case 1:
	
		hires(sqr_spec,  lambda, nwin, num_freqs, amu);
	        get_F_values(ReSpec, ImSpec, num_freqs, nwin, fv, tapsum);
 

 
           for (i = 0; i < num_freqs; i++) {
		ospec[i] =amu[i];
                 dof[i] = nwin-1;
                 Fvalues[i] = fv[i];
                  }

		break;

	case 2:


		/* get avar = variance*/

		n1 = 0;
		n2 = npoints;


                  avar = 0.0;

		for (i = n1; i < n2; i++)
			avar += (data[i]) * (data[i]);


		switch (inorm) {
		case 0:
			avar = avar / npoints;
			break;

		case 1:
			avar = avar / (npoints * npoints);
			break;

		case 2:
			avar = avar * dt * dt;
			break;

		case 3:

			avar = avar / npoints;
			break;

		default:
			break;
		}

		 


		dcf = djector((long)0,(long) num_freq_tap);
		degf = djector((long)0,(long) num_freqs);

	


		adwait(sqr_spec, dcf, lambda, nwin, num_freqs, amu, degf, avar);

                get_F_values(ReSpec, ImSpec, num_freqs, nwin, fv, tapsum);



                 /* rap up   */

           for (i = 0; i < num_freqs; i++) {
		ospec[i] =amu[i];
                 dof[i] = degf[i];
                 Fvalues[i] = fv[i];
                  }



                


		break;
	}




}


/** FUNC DEF */ void  CALL_Mspec(double *data, int *anpoints, int *akind,
				int *anwin, double *anpi, int *ainorm, double *adt, 
				double *ospec, double *dof, 
				double *Fvalues, int *aklen, double  *ReSpec, double   *ImSpec)
{

   int  npoints,  kind,  nwin, inorm, klen;
   double npi, dt;

   npoints=(double)(*anpoints);

   kind=(int)(*akind);
   nwin=(int)(*anwin);
   npi=(double)(*anpi);
   inorm=(int)(*ainorm);
   dt=(double)(*adt);
   klen=(int)(*aklen);

   Mtap_spec(data, npoints,  kind,
	     nwin,  npi,  inorm,  dt, ospec, dof, 
	     Fvalues, klen, ReSpec, ImSpec);
}


/** FUNC DEF */ void  CALL_slepian(int *anpoints,int *inwin, double *dnpi, double *tapers)
{
  int k;
  double         *lambda;
  double           *tapsum;
  int len_taps;
  double npi;
  int nwin;
  int npoints;


   npoints=(double)(*anpoints);
   nwin =(int)(*inwin);
   npi = (double)(*dnpi);


  lambda = djector((long)0, (long)nwin);
  tapsum=djector((long)0,(long)nwin);
  len_taps = npoints * nwin;


  /* tapers = djector((long)0,(long) len_taps); */
  k = multitap(npoints, nwin, lambda,  npi, tapers, tapsum);
#if 0

  tap_file = fopen("taper_file", "w");
  /* print out tapers for curiosity  */
  for(i=0; i<npoints; i++){
    for(j=0; j<nwin; j++)Rprintf(tap_file,"%15.10f ",tapers[i+j*npoints]);
    Rprintf(tap_file,"\n");
  }
  fclose(tap_file);
#endif

  /* free_djector(lambda,(long) 0,(long) nwin); */

  /* 	free_djector(tapers,(long) 0, (long)len_taps); */
 /*  free_djector(tapsum,(long) 0, (long)nwin); */

}

#if 0
/** FUNC DEF */ void  CALL_FINDWFREQ(double *data, int *anpoints, double *adt, int   *JOUT)
{

   int  npoints,  kind,  nwin, inorm, klen;
   double npi, dt;

   double *tdata;

   npoints=(double)(*anpoints);


      kind=2;  
      nwin=5;  
      npi=3; 
      inorm=0;

   dt=(double)(*adt);



   klen=100;

   Mtap_spec(data, npoints,  kind,
	     nwin,  npi,  inorm,  dt, ospec, dof, 
	     Fvalues, klen, ReSpec, ImSpec);
}
#endif


/*************************************************************************************/
/*************************************************************************************/
/*************************************************************************************/
