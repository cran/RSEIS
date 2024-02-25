#include <stdio.h>
#include <stdlib.h>
#include <R.h>


#include <math.h>
#include <string.h>

/*
#ifdef MAC
#include <sys/malloc.h>
#else
#include <malloc.h>
#endif
*/



/*********************************/
/*********************************/
/*********************************/
/** FUNC DEF  **/ void   wletxcorfreq(double *LET, int Nlet, double *sig, int Nsig, double *xcor)
{
  int i,j;
   /* int K; */
   double xx, temp, tempA, tempLET;
   double sumLET, sumA, sum2LET, sum2A;
   /* double meanLET; */
   double plop;

   /* K = (Nlet/2)+1; */

   temp = 0.0;
   tempA = 0.0;
   tempLET = 0.0;
   /* meanLET = 0.0; */
   sumLET = 0.0;
   sum2LET = 0.0;
   
   for(j=0; j<Nlet; j++)
   {
      sumLET += LET[j];
      sum2LET += LET[j]*LET[j];
   }
   
   tempLET = sqrt(Nlet*sum2LET-(sumLET*sumLET));



   for(i=0; i<(Nsig-Nlet-1);i++)
   {
      
      temp = 0.0;
      sumA = 0.0;
      sum2A = 0.0;
      tempA = 0.0;
/* calculate the RMS for the TARGET and the Cross Correlation  */
      
      for(j=0; j<Nlet; j++)
      {
	 temp += LET[j]*sig[i+j];
	 sumA +=  sig[i+j];
	 sum2A +=  sig[i+j]*sig[i+j];
	 
      }


      plop = Nlet*sum2A-(sumA*sumA);
      if(plop>= 0) tempA = sqrt(Nlet*sum2A-(sumA*sumA));

      if((tempLET*tempA)>0) 
      {
      xx = (Nlet*temp - sumA*sumLET)/(tempLET*tempA);
      }
      else
      {
	 xx = 0.0;
      }

      xcor[i] = xx;
      
   }
   
  
}

/*************************************************************************************/
/*************************************************************************************/
/** FUNC DEF  **/ void   wletxcor(double *LET, int Nlet, double *sig, int Nsig, double *xcor)
{
  int i,j;
   /* int K; */
   double xx, temp, tempA, tempLET;
   double sumLET, sumA, sum2LET, sum2A;
   /* double meanLET; */
   double plop;

   /* K = (Nlet/2)+1; */

   temp = 0.0;
   tempA = 0.0;
   tempLET = 0.0;
   /* meanLET = 0.0; */
   sumLET = 0.0;
   sum2LET = 0.0;
   
   for(j=0; j<Nlet; j++)
   {
      sumLET += LET[j];
      sum2LET += LET[j]*LET[j];
   }
   
   tempLET = sqrt(Nlet*sum2LET-(sumLET*sumLET));


 
   for(i=0; i<(Nsig-Nlet-1);i++)
   {
      
      temp = 0.0;
      sumA = 0.0;
      sum2A = 0.0;
      tempA = 0.0;
/* calculate the RMS for the TARGET and the Cross Correlation  */
      
      for(j=0; j<Nlet; j++)
      {
	 temp += LET[j]*sig[i+j];
	 sumA +=  sig[i+j];
	 sum2A +=  sig[i+j]*sig[i+j];
	 
      }


      plop = Nlet*sum2A-(sumA*sumA);
      if(plop>= 0) tempA = sqrt(Nlet*sum2A-(sumA*sumA));

      if((tempLET*tempA)>0) 
      {
      xx = (Nlet*temp - sumA*sumLET)/(tempLET*tempA);
      }
      else
      {
	 xx = 0.0;
      }

      xcor[i] = xx;
      
   }
   
  
}
/*************************************************************************************/
/*************************************************************************************/

/** FUNC DEF */ void   CALL_WLETXCOR(double *LET, int *Nlet, double *sig, int *Nsig, double *xcor)
{
   int ki, kj;

   kj = *Nsig;
   ki = *Nlet;
   wletxcor(LET, ki, sig, kj,  xcor);


 return;
}



