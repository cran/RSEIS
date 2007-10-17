
#include <stdio.h>
#include <stdlib.h>
#include <math.h>

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

void anrerror(char error_text[])
/* Numerical Recipes standard error handler */
{
        fprintf(stderr,"Numerical Recipes run-time error...\n");
        fprintf(stderr,"%s\n",error_text);
        fprintf(stderr,"...now exiting to system...\n");
        exit(1);
}


double *advector(long nl, long nh)
/* allocate a double vector with subscript range v[nl..nh] */
{
        double *v;

        v=(double *)malloc((size_t) ((nh-nl+1+NR_END)*sizeof(double)));
        if (!v) anrerror("allocation failure in advector()");
        return v-nl+NR_END;
}
void free_advector(double *v, long nl, long nh)
/* free a double vector allocated with advector() */
{
        free((FREE_ARG) (v+nl-NR_END));
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
        double     tscore,  ascore, bscore;

//	FILE           *fopen();
//	FILE           *fout = fopen("test.reg.out", "w");
		
	score_old = 1e99;
	
	dtempy = advector((long) 0, (long) num_freqs);
	dtempx = advector((long) 0, (long) num_freqs);
	
	
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


		ascore = 0.0;
		bscore = 0.0;
                score = 0.0;
                tscore = 0.0;

		for (j = i1; j <= i2; j++)
		{
		   score += (ave - y[j])*(ave - y[j]);
		   ascore += (ave - y[j])*(ave - y[j]);
		   
		}

		 tscore =score;
	
		for (j = 0; j < num ; j++) {

		   
			a = slope * dtempx[j] + intercept;
			tscore += (a - dtempy[j])*(a - dtempy[j]);
			bscore += (a - dtempy[j])*(a - dtempy[j]);
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

	//	fprintf(stderr, "FINAL:corn ave slope intercept: %d\n", KINDEX);
	//	fprintf(stderr, "%f %f %f %f\n",*corn, *final_ave, *final_slope, *final_intercept);

	free_advector(dtempx, (long) 0, (long) num_freqs);
	free_advector(dtempy, (long) 0, (long) num_freqs);

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

/*if(alpha == -1. || alpha == 1.0)
  fprintf(stderr,"alpha=%f tstar=%e e2ft=%e freq=%e a1=%e\n",alpha, tstar, e2ft,freq,a1);
*/
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
   
   double PI, f,    fc ,  tstar0;
   /* double PI, f, ftem, fa, fb, fc , tem, tstar0,  frat; */


  /*  double ave, slope, interc; */
   double alpha;
   double ds;
   double omega0;
   double bruney;
   /* double blog; */

    PI = 3.14159265358979;

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
	   /* fprintf(stderr, "TEMP:  %lf %lf %lf %lf\n", gamma, tstar0, oscore, score); */
	  if(score<oscore)
	  {
	     oscore = score;

	     dgam[2]=gamma ;
	     dstar[2] = tstar0;
	     Ktot++;
	     /* fprintf(stderr, "DMIN: %d %lf %lf %lf\n", Ktot, dstar[2], dgam[2], score); */
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
    
/*
    fprintf(stderr, "BRUNE: omega0,  tstar0, fc, alpha, gamma\n");
    fprintf(stderr, "BRUNE: %lf %lf %lf %lf %lf\n", omega0,  tstar0, fc, alpha, gamma);
    fprintf(stderr, "FINAL: %d %lf %lf\n", Ktot, dstar[2], dgam[2]);
*/

   /*  fprintf(stderr, "FOR.R:\n omega0=%g; tstar0=%lf; fc=%lf; alpha=%lf; gamma=%lf;\n", omega0,  tstar0, fc, alpha, gamma); */

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



