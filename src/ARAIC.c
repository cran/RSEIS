#include <math.h>
#include <stdlib.h>
#include <stdlib.h>

/*
#ifdef MAC
#include <sys/malloc.h>
#else
#include <malloc.h>
#endif
*/




#include <stdio.h>
#include <stdlib.h>

/* static float sqrarg; */
/* static double  dsqrarg; */

#define NR_END 1
#define FREE_ARG char*
#define SQR(a) ((sqrarg=(a)) == 0.0 ? 0.0 : sqrarg*sqrarg)
#define DSQR(a) ((dsqrarg=(a)) == 0.0 ? 0.0 : dsqrarg*dsqrarg)

void nrerror(char error_text[])
/* Numerical Recipes standard error handler */
{
        fprintf(stderr,"Numerical Recipes run-time error...\n");
        fprintf(stderr,"%s\n",error_text);
        fprintf(stderr,"...now exiting to system...\n");
        exit(1);
}

double *dvector(long nl, long nh)
/* allocate a double vector with subscript range v[nl..nh] */
{
        double *v;

        v=(double *)malloc((size_t) ((nh-nl+1+NR_END)*sizeof(double)));
        if (!v) nrerror("allocation failure in dvector()");
        return v-nl+NR_END;
}

void free_dvector(double *v, long nl, long nh)
/* free a double vector allocated with dvector() */
{
        free((FREE_ARG) (v+nl-NR_END));
}

void memcof(double  data[], int n, int m, double  *xms, double  d[])
{
        int k,j,i;
        double  p=0.0,*wk1,*wk2,*wkm;

        wk1=dvector(1,n);
        wk2=dvector(1,n);
        wkm=dvector(1,m);
        for (j=1;j<=n;j++) p += (data[j])*(data[j]);
        *xms=p/n;
        wk1[1]=data[1];
        wk2[n-1]=data[n];
        for (j=2;j<=n-1;j++) {
                wk1[j]=data[j];
                wk2[j-1]=data[j];
        }
        for (k=1;k<=m;k++) {
                float num=0.0,denom=0.0;
                for (j=1;j<=(n-k);j++) {
                        num += wk1[j]*wk2[j];
                        denom += (wk1[j])*(wk1[j])+(wk2[j])*(wk2[j]);
                }
                d[k]=2.0*num/denom;
                *xms *= (1.0-(d[k])*(d[k]));
                for (i=1;i<=(k-1);i++)
                        d[i]=wkm[i]-d[k]*wkm[k-i];
                if (k == m) {
                        free_dvector(wkm,1,m);
                        free_dvector(wk2,1,n);
                        free_dvector(wk1,1,n);
                        return;
                }
                for (i=1;i<=k;i++) wkm[i]=d[i];
                for (j=1;j<=(n-k-1);j++) {
                        wk1[j] -= wkm[k]*wk2[j];
                        wk2[j]=wk2[j+1]-wkm[k]*wk1[j+1];
                }
        }
        nrerror("never get here in memcof.");
}
#undef NRANSI

double  evlmem(double  fdt, double  d[], int m, double  xms)
{
        int i;
        double  sumr=1.0,sumi=0.0;
        double wr=1.0,wi=0.0,wpr,wpi,wtemp,theta;

        theta=6.28318530717959*fdt;
        wpr=cos(theta);
        wpi=sin(theta);
        for (i=1;i<=m;i++) {
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

     for(i=p+1 ; i<n; i++)
     {
	pred = 0;
	for(j=1 ; j<=p   ; j++)
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
   
   int  IW;

  
   double *z1;
   double *win1;
   double  *cof1, *cof2, pm;

   int  k1, k2;
   int p1, p2, q1, q2, n1, n2;
   int N, K, M;
   double mz1;

   double s1, s2;

   double kdat;
   int kmin;

   M = p;
  
   N = num_points;
    
   num_cof = M;

   IW = floor(W/deltat + 0.5);    /** round the wind length */

/***********************************/

   cof1 = dvector(1,M);
   cof2 = dvector(1,M);

   z1 = dvector(0,N);

/***********************************/

/*   select window for noise part  window 1 */


   k1=T1-floor(O1/deltat)-IW;
   k2 = k1+IW;

   /* fprintf(stderr, "1 IW, k1, k2: %d %d %d\n", IW, k1, k2); */

   win1  = dvector(0, IW);
   for(i=0; i<IW; i++) {  win1[i] = y1[k1+i]; }

   memcof(win1-1, IW, num_cof, &pm, cof1);

   /*
     fprintf(stderr, "\n");
     for(i=1; i<=num_cof; i++)
     {
     fprintf(stderr, "%d %f\n",i, cof1[i]);
     }
   */

/***********************************/

/*   select window for signal part  window 2  */
   
   k1=T1+floor(O2/deltat)+1;
   k2 = k1+IW;

   /* fprintf(stderr, "2 IW, k1, k2: %d %d %d\n", IW, k1, k2); */


   for(i=0; i<IW; i++) {  win1[i] = y1[k1+i]; }

   memcof(win1-1, IW, num_cof, &pm, cof2);

/*
   fprintf(stderr, "\n");
 for(i=1; i<=num_cof; i++)
   {
      fprintf(stderr, "%d %f\n",i, cof2[i]);
   }
*/

/***********************************/


   k1 = 2*M+1;
   k2 = N-(2*M+1);
  
   kdat = 0;
   kmin = 0;

   for(K=k1; K<=k2; K++)
    {

       p1 = M;
       p2 = K+1;
       q1 = K;
       q2 = N-M;
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


   free_dvector(cof1, 1, M);
   free_dvector(cof2, 1, M);
   free_dvector(z1, 0, N);

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
   cof = dvector(1,num_cof);
   memcof(data-1, num_points, num_cof, &pm, cof);


   for(i=0; i< num_freqs; i++)
   {
      fdt = 0.5*i/num_freqs;
      f0 = evlmem( fdt, cof, num_cof, pm);
      series[i] =  norm*f0;
      
   }

  free_dvector(cof, 1,num_cof);


  return(0);

}

/** FUNC DEF */ void   CALL_ARspec(double *y1, double *kout, int *inum,  int *p,  int *numc)
{

  int num_points, numcof,  klength, m ;
 
   num_points = *inum;
   klength = *p;
   numcof = *numc;
 
   m = autoreg_spec(y1, kout, num_points, klength, numcof );


 return;
}



/********************************************


make drive_araic
R CMD SHLIB ARAIC.o

dyn.load("/home/lees/Progs/Rc/ARAIC.so")




************************************************/
