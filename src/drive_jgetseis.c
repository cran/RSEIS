#include <stdio.h>
#include <rpc/rpc.h>

#include <assert.h>
#include <math.h>
#include <string.h>
#include <time.h>
#include <stdlib.h>
#include <unistd.h>
#include <stddef.h>
#include <math.h>
#include <sys/stat.h>

#include <malloc.h>

#include "seis.h"

#include "sac.h"

#define      USE_AH_XDR 0

#if USE_AH_XDR
#include "ahhead.h"
#endif

void  get_one_segy(char *fn, TIME_SERIES *ts);
void  get_one_sac(char *fn, TIME_SERIES *ts);

int main(int argc, char **argv)
{

  TIME_SERIES ts;
  /* char fn; */

  int i, j, i1, i2, k, kind;
  double dt, tt;
  double rms;
  double sec2;
   
  int n, m;

  double wt;  /* half window time in seconds */
  double skipt; 

  int jfile;

  char infile[100];


  wt = 2*60;
  skipt = 3*60;

  kind = atoi(argv[1]);


  fprintf(stderr, "KIND = %d argc=%d\n", kind, argc);
  for(jfile=2; jfile<(argc); jfile++)
    {
      fprintf(stderr, "GOING to get data %d fn=%s\n",jfile, argv[jfile]);
       
    }
  fprintf(stderr, "++++++++++++++++++++++++++++++++++++++++++++\n");

  for(jfile=2; jfile<(argc); jfile++)
    {

      strcpy(infile, argv[jfile]);

      fprintf(stderr, "GOING to get data %d fn=%s\n",jfile, infile);

      switch(kind)
	{
	case 1:
	  get_one_segy(infile,  &ts);
	  break;
	case 2:
	  get_one_sac(infile,  &ts);
	  break;

	case 3:

 fprintf(stderr, "AH format not available from this routine\n");

#if USE_AH_XDR
	  get_one_ah(infile,  &ts);
#endif

	  break;

	default:
	  get_one_segy(infile,  &ts);
	  break;
	}
   

      fprintf(stderr, " GOT the data, testing here: %d\n", jfile);


      /* get_one_segy(argv[j],  &ts); */

      n = ts.numsamp;
      dt = ts.deltat;
      tt = n*dt;

      i1 = (int)(wt/dt);
      i2 = (int)(skipt/dt);

      fprintf(stderr, "name=<%s> comp=<%s> %d %d %d %d %f %f\n",  ts.id.staname,  ts.id.comp ,  
	      ts.year,  ts.jday, ts.hour,  ts.minute,   ts.sec, ts.deltat);
      for(i=i1; i<n; i+=i2)
	{
	  rms = 0;
	  m=0;
	  sec2 = i*dt;
	  for(k=i-i1; k<i+i1; k++)
	    {
	      if(k>n) break;
	      rms += ts.amp[k]*ts.amp[k];
	      m++;
	    }   
	  if(m>0)
	    {
	      rms /=m;
	    }
	  fprintf(stdout, "%d %d %d %d %f %f %g\n",  ts.year,  ts.jday, ts.hour,  
		  ts.minute,   ts.sec, sec2, rms);
	}
   
      free(ts.amp);
      /*  free(ts.ampbak); */
   
      fprintf(stderr, "free the data: %d\n", jfile);
 fprintf(stderr, "====================================================\n");

   
    }
  return(0);


}


/**************
this works:

cd /data/wadati/bourbon/DATA/HELENS/200409/20040924_195000_MAN
/home/lees/Progs/R_PAX/RSEIS/src/drive_jgetseis 2 *.UW



##  this does not:
cd /home/lees/Site/Santiaguito/Wave/KGLe3d/TEST1

/home/lees/Progs/R_PAX/RSEIS/src/drive_jgetseis 2 DOM.009.z

************/

