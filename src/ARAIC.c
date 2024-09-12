#include <math.h>
#include <stdlib.h>
#include <R.h> 

/*
#ifdef MAC
#include <sys/malloc.h>
#else
#include <malloc.h>
#endif
*/

/* static float sqrarg; */
/* static double  dsqrarg; */

#define NR_END 1
#define FREE_ARG char*
#define SQR(a) ((sqrarg=(a)) == 0.0 ? 0.0 : sqrarg*sqrarg)
#define DSQR(a) ((dsqrarg=(a)) == 0.0 ? 0.0 : dsqrarg*dsqrarg)


double *dvector(long nl)
/* allocate a double vector with subscript range v[nl..nh] */
{
        double *v;
	v=(double *)R_Calloc((size_t) (nl) , double);
	
        if (!v) Rprintf("allocation failure in dvector()");
        return v;
}

void free_dvector(double *v)
/* free a double vector allocated with dvector() */
{
        /* free((FREE_ARG) (v+nl-NR_END)); */
  R_Free( v );
}

/** FUNC DEF */ void memcof(double  data[], int n, int m, double  *xms, double  d[])
{
  int k,j,i;
        double  p=0.0,*wk1,*wk2,*wkm;
	double num=0.0,denom=0.0;

	/* REprintf("ALLOC memcof 0: n=%d m=%d \n", n, m ); */
        wk1=dvector(n);
        wk2=dvector(n);
        wkm=dvector(m);
	/* REprintf("DONE ALLOC 0\n"); */
	
        *xms=p/n;
        wk1[0]=data[0];
        wk2[n-2]=data[n-1];
        for (j=1;j<(n-1);j++) {
                wk1[j]=data[j];
                wk2[j-1]=data[j];
        }


	
	
	
        for (k=0;k<m;k++) {
	  
	  num=0.0; denom=0.0;
                for (j=0;j<(n-k);j++) {
                        num += wk1[j]*wk2[j];
                        denom += (wk1[j])*(wk1[j])+(wk2[j])*(wk2[j]);
                }
		
		

                d[k]=2.0*num/denom;
                *xms *= (1.0-(d[k])*(d[k]));
		
                for (i=0;i<(k-1);i++)
		  {
		   	
		    d[i]=wkm[i]-d[k]*wkm[k-i];


		  } 
			

			
                if (k == (m) ) {
		  /* REprintf("FREE memcof 1: %d %d \n", k, m); */
                        free_dvector(wkm); 
                        free_dvector(wk2);
                        free_dvector(wk1);
			/* REprintf("DONE FREE memcof 1 \n"); */
                        return;
                }
		

		 for (i=0;i<k;i++) { wkm[i]=d[i]; }

		
		
                for (j=0;j<(n-k-1);j++) {
		  
                        wk1[j] -= wkm[k]*wk2[j];
                        wk2[j] =  wk2[j+1]-wkm[k]*wk1[j+1];
                }
	
        }
        /* nrerror("never get here in memcof."); */
}
/*#undef NRANSI*/

/** FUNC DEF */  double  evlmem(double  fdt, double  d[], int m, double  xms)
{
        int i;
        double  sumr=1.0,sumi=0.0;
        double wr=1.0,wi=0.0,wpr,wpi,wtemp,theta;

        theta=6.28318530717959*fdt;
        wpr=cos(theta);
        wpi=sin(theta);
        for (i=0;i<m;i++) {
                wr=(wtemp=wr)*wpr-wi*wpi;
                wi=wi*wpr+wtemp*wpi;
                sumr -= d[i]*wr;
                sumi -= d[i]*wi;
        }
        return xms/(sumr*sumr+sumi*sumi);
}


/** FUNC DEF */ double  ZAR(double *ar, int p, double *x, int n)
  {
    
     int i, j; 
     double pred, res, sum;
     double xint;
     
     xint = 0;

     /* xint = mean(x) */


     sum = 0;
     /* REprintf("W=%d n=%d\n", p, n); */
     for(i=p ; i<(n-1); i++)
     {
	pred = 0;
	for(j=0 ; j<p   ; j++)
	{
	   pred += (ar[j] * x[i - j - 1]) + xint;
	}
	res = x[i] - pred;
	sum += res*res;
     }

       return(sum);
  }

/***************************************************/

/** FUNC DEF */ double mean(double *y, int n)
{
   int i;
   double m;
   m = 0;
   for(i=0; i<n; i++)
   {
      m+=y[i];
   }
   m/=n;
   return(m);
}


/***************************************************/

/** FUNC DEF */ int  araic(double *y1, int num_points, double deltat, int p, 
			   int T1, double O1, double O2, double W, double *kout)
{
   int  i, j;
   int num_cof;
  
  
  
/*****  parameters for AR-AIC   ****/
   
   int  IW=1;

   double *z1;
   double *win1;
   double  *cof1, *cof2, pm;

   int  k1, k2;
   int p1, p2,  n1, n2;
   int N, K, M;
   double mz1;

   double s1, s2;

   
   int kmin;

   
   M = p;
  
   N = num_points;
    
   num_cof = M;
   /* REprintf("W=%lf deltat=%lf\n", W, deltat); */
   IW = floor(W/deltat + 0.5);    /** round the wind length */
   /* REprintf("IW = %d\n", IW); */
/**************** memory *******************/
 /* REprintf("ALLOC 1.1\n"); */
   cof1 = dvector(M);
   /* REprintf("ALLOC 1.2\n"); */
   cof2 = dvector(M);
   /* REprintf("ALLOC 1.3\n"); */
   z1 = dvector(N);
/* REprintf("ALLOC: 4 vectors IW=%d\n", IW); */
   win1  = dvector(IW);
/* REprintf("ALLOC 3 vectors\n"); */
/***********************************/

/*   select window for noise part  window 1 */


   k1=T1-floor(O1/deltat)-IW;
   k2 = k1+IW;
   if(k1<0)
     {
         /* REprintf("FREE 2\n"); */
       free_dvector(cof1);
       free_dvector(cof2);
       free_dvector(z1);
       free_dvector(win1);
       return(-1);
     }
   if(k2>num_points)
     {
         /* REprintf("FREE 3\n"); */
       free_dvector(cof1);
       free_dvector(cof2);
       free_dvector(z1);
       free_dvector(win1);
       return(-1);
     }
   
   

   
   
   for(i=0; i<IW; i++) {  win1[i] = y1[k1+i]; }
   
   memcof(win1, IW, num_cof, &pm, cof1);

   
/***********************************/

/*   select window for signal part  window 2  */
   
   k1=T1+floor(O2/deltat)+1;
   k2 = k1+IW;
   if(k2>num_points) {
       /* REprintf("FREE 4\n"); */
     free_dvector(cof1);
     free_dvector(cof2);
     free_dvector(z1);
     free_dvector(win1);
     
     return(-1);

   }

   /* Rprintf("k1=%d k2=%d IW=%d N=%d\n", k1, k2, IW, N); */

   for(i=0; i<IW; i++) {  win1[i] = y1[k1+i]; }

   memcof(win1, IW, num_cof, &pm, cof2);


/***********************************/


   k1 = 2*M+1;
   k2 = N-(2*M+1);
  
  
   kmin = 0;
 /* REprintf("READY 4\n"); */
   for(K=k1; K<=k2; K++)
    {

       p1 = M;
       p2 = K+1;
       
       n1 = K-M;
       n2 = N-M-K;

       if(n1<2 || n2<2 ) continue;
     /* ##  left window */
       mz1 = 0;
       for(j=0; j<n1; j++) 
       { 
	  z1[j] = y1[p1+j];
	  mz1+=z1[j];
       }
       mz1/=n1;
       /** subtract out mean  */
       for(j=0; j<n1; j++) 
       { 
	  z1[j]-=mz1;
	 
       }
      
	 s1 = ZAR(cof1, M, z1, n1);

     /* ##  right  window */
       mz1 = 0;
       for(j=0; j<n2; j++) 
       { 
	  z1[j] = y1[p2+j];
	  mz1+=z1[j];
       }
       mz1/=n2;
        /** subtract out mean  */
       for(j=0; j<n2; j++) 
       { 
	  z1[j]-=mz1;
	 
       }
      
	 s2 = ZAR(cof2, M, z1, n2);
     
	 if(s1>0.0 && s2>0) 
	 {
	    kout[K] = n1*log(s1) + n2*log(s2);
	 }
	 else
	 {
	    kout[K] = 0.0;
	 }

    }

  /* REprintf("END of CODE FREE 6\n"); */
   free_dvector(cof1);
   /* REprintf("FREE 6.1\n"); */
   free_dvector(cof2);
   /* REprintf("FREE 6.2\n"); */
 
   free_dvector(z1);
    
   /* REprintf("FREE 6.3\n"); */
   free_dvector(win1);
   /* REprintf("DONE 6.4\n"); */

   return(kmin);


}



/** FUNC DEF */ void   CALL_ARAIC(double *y1, int *inum, double *deltat, int *p, 
			   int *iT1, double *iO1, double *iO2, double *iW, double *kout)
{

   int num_points, M, T1 ;
   double dt ;
   double O1,  O2, W;

  


   num_points = *inum;
   dt = *deltat;
   M = *p;
   T1 = *iT1;
   O1 = *iO1;
   O2 = *iO2;
   W = *iW;

  araic(y1, num_points, dt, M, 
       T1, O1, O2, W, kout);

 return;
}
/* FUNC DEF */ int autoreg_spec(double *data, double *series, int inum, int klength, int numcof)
{

   int i;
/*  int i, isign=1; */


   /* double *amp; */
   int num_freqs;
   
   int  num_points;
   int  num_cof;
   /* int  num_cof, freqwin, numco; */

   double *cof, pm;
   double f0, fdt;
   double norm;

   num_points = inum;
   
   if(numcof<2) 
   {  num_cof = 100; }
   else
   {  num_cof = numcof; }
   

   /* num_cof =numco+(j)*(num_points-numco) /9.; */
   num_freqs = klength;
  

   norm = 1/(double)num_freqs;
   
   cof = dvector(num_cof);
   
   memcof(data-1, num_points, num_cof, &pm, cof);


   for(i=0; i< num_freqs; i++)
   {
      fdt = 0.5*i/num_freqs;
      f0 = evlmem( fdt, cof, num_cof, pm);
      series[i] =  norm*f0;
      
   }
  /* REprintf("FREE 7\n"); */
   free_dvector(cof); 


  return(0);

}

/** FUNC DEF */ void   CALL_ARspec(double *y1, double *kout, int *inum,  int *p,  int *numc)
{

  int num_points, numcof,  klength ;
 
   num_points = *inum;
   klength = *p;
   numcof = *numc;
 
    autoreg_spec(y1, kout, num_points, klength, numcof );


 return;
}



/********************************************


make drive_araic
R CMD SHLIB ARAIC.o

dyn.load("/home/lees/Progs/Rc/ARAIC.so")




************************************************/
