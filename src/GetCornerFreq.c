
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <R.h>

/*
#ifdef MAC
#include <sys/malloc.h>
#else
#include <malloc.h>
#endif
*/




#define SMALL_NUMBER 1e-300

/**

driver for corner frequency stuff in R

cd /home/lees/Progs/Rc
make drive_corn

R CMD SHLIB GetCornerFreq.o


example:  R

dyn.load("/home/lees/Progs/Rc/GetCornerFreq.so")

gc = get.corner(  log10(Spec$f) , log10(lspec), dt, f1, f2)



**/

#define NR_END 1
#define FREE_ARG char*


double *advector(long nl)
/* allocate a double vector with subscript range v[nl..nh] */
{
        double *v;

        /* v=(double *)malloc((size_t) ((nh-nl+1+NR_END)*sizeof(double))); */
	/* v=(double *) (size_t) R_alloc(nh-nl+1+NR_END , sizeof(double)); */
	v=(double *) (size_t) R_alloc(nl , sizeof(double)); 

        if (!v) Rprintf("allocation failure in advector()");
        return v;
}
void free_advector(double *v)
/* free a double vector allocated with advector() */
{
        R_Free( v);
}



/** FUNC DEF */void dwindow_trace( double  input[], double  output[], int start, int num)
{
   int i, j;
   for(i=start, j=0; i< start+num ;  i++, j++)
   {
      output[j] = input[i];
   }
}


/** FUNC DEF */void dget_abfit(double *x, double *y, int length, double *slope, double *intercept)
{   

      double s=0.0, sx=0.0, sy=0.0, sxx=0.0, sxy=0.0;
      double del;
      int i;
      
      for (i = 0; i < length; i++)
      
        {
        
        sx += x[i];
        sy += y[i];
        sxx += x[i] * x[i];
        sxy += x[i]*y[i];
        } 
      
       s = length;
       
       del = s*sxx - sx*sx;
       
       if(del != 0.0)
       {
            *intercept = (sxx*sy - sx*sxy)/del;
            *slope = (s*sxy - sx*sy)/del;
         }
       
}
/** FUNC DEF */ int  CALL_DLINE(double *x, double *y, int *length, double *slope, double *intercept)
{

   int n;
   

   n = (int)(*length);
 
   dget_abfit(x, y, n, slope, intercept);

   return(1);

}


/** FUNC DEF */ double  dget_corner(double *x, double *y, int num_freqs, 
				    int *K, double *final_ave,
				    double *final_slope, double *final_intercept)
{

/**  find corner freq, omega0 and slope of line
    on LOG-LOG plot of seismic source record (displacement)
x = log10 frequency
y = log10 displacement spectrum
**/
	int             i, j, k, KINDEX;
	int             i1, i2, i3, num;
	double           ave,  a;
	double          *dtempy, *dtempx;
	double           slope, intercept;
	double          score, score_old;
       

//	FILE           *fopen(const char *filename, const char *mode);
//	FILE           *fout = fopen("test.reg.out", "w");
		
	score_old = 1e99;
	
	dtempy = advector((long) num_freqs);
	dtempx = advector( (long) num_freqs);
	
	
	i1 = 0;            //  initial point
	i3 = num_freqs-1;  // final point 
	KINDEX = 0;
	
	for (k = 1 ; k < num_freqs-1 ; k++) 
	{
	   
	   i2 = k;
	  
	   ave = 0.0;

		for (i = i1; i < i2; i++) {
			ave += y[i];
		}
		ave = ave / (i2 - i1 + 1);
		num  = i3 - i2+1;

		dwindow_trace(y, dtempy, i2, num);
		dwindow_trace(x, dtempx, i2, num);

		dget_abfit(dtempx, dtempy, num, &slope, &intercept);


		
                score = 0.0;
             

		for (j = i1; j <= i2; j++)
		{
		   score += (ave - y[j])*(ave - y[j]);
		   
		   
		}

		
	
		for (j = 0; j < num ; j++) {

		   
			a = slope * dtempx[j] + intercept;
			
		}



		for (j = i2+1; j <= i3; j++) {
			a = slope * x[j] + intercept;
			score += (a - y[j])*(a - y[j]);
		}

		

		if (score < score_old) {
			score_old = score;
			*final_ave = ave;
			*final_slope = slope;
			*final_intercept = intercept;
			KINDEX = k;
			
		
		}

	
	}

	*K = KINDEX;


       return score_old;


}



/** FUNC DEF */ void CALL_DCORN(double *x, double *y, int  *num_freqs, 
	      int  *K, double *final_ave,
	      double *final_slope, double *final_intercept)
{

   int n;
 
   n = (int)(*num_freqs);

   dget_corner(x, y, n, 
	       K, final_ave,
	       final_slope, final_intercept);

}
/*****************************************************************/
/*****************************************************************/
double brune_func(double freq, double omega0, double tstar0, double fc,
		  double alpha, double gamma)
{
   double       pi=3.141592654;
   double  gam2;
   double tstar, a1, a2;
   double e2ft;
   double tmod;

   gam2 = 2.0*gamma;
   a1 = pow( (double)freq,(double)(-alpha));
   a2 = pow( (double) (freq/fc),(gam2) );

   tstar = tstar0* a1 ;
   e2ft = exp((double)(-pi*freq*tstar));


   tmod = (omega0*e2ft) / sqrt(1+a2 );

   if(tmod == 0.0) tmod = SMALL_NUMBER;

   return(tmod);

}

/** FUNC DEF */ double  dget_gamma(double *x, double *y, int *num_freqs, 
				    double *fcorn, double *omega,
				   double dgam[3], int *ngam,
				   double dstar[3], int *nstar)
{

   int i, igam, ng, nt, nf;
   int istar, Ktot;
   double gamma,  dg;
 /*   double gam,gamma, g1, g2, dg; */

   double score, oscore;
   
   double  f,    fc ,  tstar0;
   /* double PI, f, ftem, fa, fb, fc , tem, tstar0,  frat; */


  /*  double ave, slope, interc; */
   double alpha;
   double ds;
   double omega0;
   double bruney;
   /* double blog; */

  

    alpha = 0;
    tstar0 = dstar[0];
    nt = *nstar;
    ds = (dstar[1] -  dstar[0])/ (double) nt ;

    nf = *num_freqs;

    gamma = dgam[0];
    ng = *ngam;
    dg = (dgam[1] -  dgam[0])/ (double) ng ;


/** set the score  **/
    score = 0;
    tstar0 = dstar[0];
    fc = *fcorn;
    gamma = dgam[0];
    omega0 = *omega;
    Ktot = 0;
    for(i=0; i<nf; i++)
    {
       f = x[i];
       bruney = brune_func(f, omega0,  tstar0, fc, alpha, gamma); 

       score += (log10(y[i]) - log10(bruney))*(log10(y[i]) - log10(bruney));

    }
    dgam[2]=gamma ;
    dstar[2] = tstar0;
    oscore = score;
/**  minimize the score  **/
    for(igam=0; igam< ng; igam++)
    {
       
       gamma = dgam[0]+(float)igam*dg;
       for(istar=0; istar<nt; istar++)
       {
	  tstar0 = dstar[0]+(float)istar*ds;
	  score = 0;
	  for(i=0; i<nf; i++)
	  {
	     
	   f = x[i];
	   bruney = brune_func(f, omega0,  tstar0, fc, alpha, gamma); 
	     
	     score += (log10(y[i]) - log10(bruney))*(log10(y[i]) - log10(bruney));
	     
	  }
	  
	  if(score<oscore)
	  {
	     oscore = score;

	     dgam[2]=gamma ;
	     dstar[2] = tstar0;
	     Ktot++;
	     
	  }
       }

    }

    tstar0=dstar[2];
    gamma = dgam[2];


    for(i=0; i<nf; i++)
    {
       
       f = x[i];
       bruney = brune_func(f, omega0,  tstar0, fc, alpha, gamma); 
       y[i] = bruney ;
    }
    

    return(Ktot);
    
    
}
/*****************************************************************/
/*****************************************************************/

/** FUNC DEF */ void CALL_DGAMMA(double *x, double *y, int *num_freqs, 
				    double *fcorn, double *omega,
				   double dgam[3], int *ngam,
				   double dstar[3], int *nstar)
{
 

   dget_gamma(x, y, num_freqs, fcorn, omega,
	      dgam, ngam,
	      dstar, nstar);
}



