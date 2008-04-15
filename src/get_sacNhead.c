#include <stdio.h>

#include <assert.h>
#include <math.h>
#include <string.h>
#include <time.h>
#include <stdlib.h>
#include <unistd.h>
#include <stddef.h>

#include "seis.h"
#include "sac.h"
/**********


make -f JMLmakefile drive_sacNhead
 

R CMD SHLIB get_sacNhead.o


**********/




/**********
	   THE SAC HEADER   ****/
#define DEBUG 0

int gblank(char *cha);


void  tojul(int year,int month,int day,int *jul);



/*****************/
/* read a sacfile, return pointer to data structure */
void read_sac(char *file ,SacHeader *sh,float  **p);

typedef struct
{

   short year, month, day, hour, minute, jday;
   double sec;

   DateTime epoch;
   double time_offset, time_shift;

   TraceID  id;
   int  filtonoff, fliponoff, pol;

   float *amp;
   float *ampbak;

   double scale_factor;

   double sensitivity_factor;
   int    sensor_flag;

   int velocity_flag;
   int scale_flag;
   int decim;
   double t1, t2;
   double origin_time;
   double reference_time;
   float deltat;
   long numsamp;
   double max,min;
   char *units;

   int numpix;
   PICK timepix[100];

  double  evla , evlo, stla, stlo, ppick, spick, otime;

  double   stel, evel;
  double   stdp, evdp;

} SAC_SERIES ;



#define LINE_LENGTH 1000


/* #include "../snaps/Src2/Utils/Uwpfif/uwpfif.h" */
/* #include "../snaps/Src2/Utils/Uwdfif/uwdfif.h" */




/*****************/
/* read a sacfile, return pointer to data structure */

/** FUNC DEF  **/ int  SACHEAD2VEC(SacHeader sh, double *dubs, int *mints , char **mchars, char **echars)
{
  int k, kchar;
  int mchar;
  char *tempchar,  *tempchar2;
  /*
  int mints[40];
  double dubs[70];
  char mchars[22][9];
  char echars[17];
  */

  tempchar = (char *) malloc ( (9) * sizeof(char)); 
  tempchar2 = (char *) malloc ( (17) * sizeof(char)); 
  

 k = 0;

  dubs[k]=sh.delta; k++;       dubs[k]=sh.depmin; k++;      dubs[k]=sh.depmax; k++;      dubs[k]= sh.scale; k++;      dubs[k]=sh.odelta; k++; 
  dubs[k]=sh.b; k++;           dubs[k]=sh.e; k++;           dubs[k]=sh.o; k++;           dubs[k]= sh.a; k++;          dubs[k]=sh.internal1; k++; 
  dubs[k]=sh.t0; k++;          dubs[k]=sh.t1; k++;          dubs[k]=sh.t2; k++;          dubs[k]= sh.t3; k++;         dubs[k]=sh.t4; k++; 
  dubs[k]=sh.t5; k++;          dubs[k]=sh.t6; k++;          dubs[k]=sh.t7; k++;          dubs[k]= sh.t8; k++;         dubs[k]=sh.t9; k++; 
  dubs[k]=sh.f; k++;           dubs[k]=sh.resp0; k++;       dubs[k]=sh.resp1; k++;        dubs[k]= sh.resp2; k++;      dubs[k]=sh.resp3; k++; 
  dubs[k]=sh.resp4; k++;       dubs[k]=sh.resp5; k++;       dubs[k]=sh.resp6; k++;       dubs[k]= sh.resp7; k++;      dubs[k]=sh.resp8; k++; 
  dubs[k]=sh.resp9; k++;       dubs[k]=sh.stla; k++;        dubs[k]=sh.stlo; k++;         dubs[k]= sh.stel; k++;        dubs[k]=sh.stdp; k++; 
  dubs[k]=sh.evla; k++;        dubs[k]=sh.evlo; k++;        dubs[k]= sh.evel; k++;       dubs[k]= sh.evdp; k++;       dubs[k]= sh.unused1; k++; 
  dubs[k]= sh.user0; k++;      dubs[k]= sh.user1; k++;      dubs[k]= sh.user2; k++;      dubs[k]= sh.user3; k++;      dubs[k]= sh.user4; k++; 
  dubs[k]= sh.user5; k++;      dubs[k]= sh.user6; k++;      dubs[k]= sh.user7; k++;      dubs[k]= sh.user8; k++;      dubs[k]= sh.user9; k++; 
  dubs[k]= sh.dist; k++;       dubs[k]= sh.az; k++;         dubs[k]= sh.baz; k++;        dubs[k]= sh.gcarc; k++;      dubs[k]= sh.internal2; k++; 
  dubs[k]= sh.internal3; k++;  dubs[k]= sh.depmen; k++;     dubs[k]= sh.cmpaz; k++;      dubs[k]= sh.cmpinc; k++;     dubs[k]= sh.unused2; k++; 
  dubs[k]= sh.unused3; k++;    dubs[k]= sh.unused4; k++;    dubs[k]= sh.unused5; k++;    dubs[k]= sh.unused6; k++;    dubs[k]= sh.unused7; k++; 
  dubs[k]= sh.unused8; k++;    dubs[k]= sh.unused9; k++;    dubs[k]= sh.unused10; k++;   dubs[k]= sh.unused11; k++;   dubs[k]= sh.unused12; k++; 


  k=0;
  mints[k]=sh.nzyear; k++;     mints[k]=sh.nzjday; k++;     mints[k]=sh.nzhour; k++;     mints[k]=sh.nzmin; k++;      mints[k]=sh.nzsec; k++; 
  mints[k]=sh.nzmsec; k++;     mints[k]=sh.internal4; k++;  mints[k]=sh.internal5; k++;  mints[k]=sh.internal6; k++;  mints[k]=sh.npts; k++; 
  mints[k]=sh.internal7; k++;  mints[k]=sh.internal8; k++;  mints[k]=sh.unused13; k++;   mints[k]=sh.unused14; k++;   mints[k]=sh.unused15; k++; 
  mints[k]=sh.iftype; k++;     mints[k]=sh.idep; k++;       mints[k]=sh.iztype; k++;     mints[k]=sh.unused16; k++;   mints[k]=sh.iinst; k++; 
  mints[k]=sh.istreg; k++;     mints[k]=sh.ievreg; k++;     mints[k]=sh.ievtyp; k++;     mints[k]=sh.iqual; k++;      mints[k]=sh.isynth; k++; 
  mints[k]=sh.unused17; k++;   mints[k]=sh.unused18; k++;   mints[k]=sh.unused19; k++;   mints[k]=sh.unused20; k++;   mints[k]=sh.unused21; k++; 
  mints[k]=sh.unused22; k++;   mints[k]=sh.unused23; k++;   mints[k]=sh.unused24; k++;   mints[k]=sh.unused25; k++;   mints[k]=sh.unused26; k++; 
  mints[k]=sh.leven; k++;      mints[k]=sh.lpspol; k++;     mints[k]=sh.lovrok; k++;     mints[k]=sh.lcalda; k++;     mints[k]=sh.unused27; k++; 

  k = 0;


  kchar = gblank(sh.kstnm);  
  mchar = 8;

  kchar = gblank(sh.kstnm);  if(kchar>mchar) kchar=mchar;
  strncpy(tempchar, sh.kstnm , kchar);
  strcpy(mchars[k], tempchar); k++;

  kchar = gblank(sh.khole); if(kchar>mchar) kchar=mchar;
  strncpy( tempchar, sh.khole, kchar);
  strcpy(mchars[k], tempchar); k++;


  kchar = gblank(sh.ko); if(kchar>mchar) kchar=mchar;
  strncpy( tempchar, sh.ko, kchar);
    strcpy(mchars[k], tempchar);   k++;  
    
  kchar = gblank(sh.ka); if(kchar>mchar) kchar=mchar;
  strncpy(tempchar , sh.ka, kchar);
   strcpy(mchars[k], tempchar);  k++;   
   
  
  kchar = gblank(sh.kt0);if(kchar>mchar) kchar=mchar;
  strncpy(tempchar , sh.kt0, kchar);
   strcpy(mchars[k], tempchar);  k++;      
     
  kchar = gblank(sh.kt1);if(kchar>mchar) kchar=mchar;
  strncpy(tempchar , sh.kt1, kchar);
    strcpy(mchars[k], tempchar);  k++;      
     
  kchar = gblank(sh.kt2 );if(kchar>mchar) kchar=mchar;
  strncpy(tempchar , sh.kt2, kchar);
    strcpy(mchars[k], tempchar); k++;     
 
  kchar = gblank(sh.kt3  );if(kchar>mchar) kchar=mchar;
  strncpy(tempchar , sh.kt3, kchar);
    strcpy(mchars[k], tempchar);  k++;      
   
  kchar = gblank(sh.kt4   );if(kchar>mchar) kchar=mchar;
  strncpy( tempchar, sh.kt4 , kchar);
    strcpy(mchars[k], tempchar); k++;      
  
  kchar = gblank(sh.kt5 );if(kchar>mchar) kchar=mchar;
  strncpy(tempchar , sh.kt5, kchar);
    strcpy(mchars[k], tempchar);  k++;      

  kchar = gblank(sh.kt6  );if(kchar>mchar) kchar=mchar;
  strncpy(tempchar , sh.kt6 , kchar);
    strcpy(mchars[k], tempchar);  k++;      
   
  kchar = gblank(sh.kt7 );if(kchar>mchar) kchar=mchar;
  strncpy(tempchar , sh.kt7 , kchar);
    strcpy(mchars[k], tempchar);  k++;      
    
  kchar = gblank(sh.kt8 );if(kchar>mchar) kchar=mchar;
  strncpy( tempchar, sh.kt8, kchar);
     strcpy(mchars[k], tempchar); k++;      

  kchar = gblank(sh.kt9  );if(kchar>mchar) kchar=mchar;
  strncpy(tempchar , sh.kt9, mchar);
   strcpy(mchars[k], tempchar);  k++;      
  
  kchar = gblank(sh.kf );if(kchar>mchar) kchar=mchar;
  strncpy( tempchar, sh.kf, kchar);
    strcpy(mchars[k], tempchar); k++;      
     
  kchar = gblank(sh.kuser0 );if(kchar>mchar) kchar=mchar;
 strncpy(tempchar , sh.kuser0, kchar);
    strcpy(mchars[k], tempchar);  k++;      

  kchar = gblank(sh.kuser1 );if(kchar>mchar) kchar=mchar;
  strncpy(tempchar , sh.kuser1, kchar);
    strcpy(mchars[k], tempchar);  k++;      

  kchar = gblank(sh.kuser2  );if(kchar>mchar) kchar=mchar;
  strncpy(mchars[k] , sh.kuser2, mchar);
    strcpy(mchars[k], tempchar);  k++;      

  kchar = gblank(sh.kcmpnm );if(kchar>mchar) kchar=mchar;
  strncpy( tempchar, sh.kcmpnm, kchar);
     strcpy(mchars[k], tempchar); k++;      

  kchar = gblank(sh.knetwk  );if(kchar>mchar) kchar=mchar;
  strncpy( tempchar, sh.knetwk, kchar);
     strcpy(mchars[k], tempchar); k++;      

  kchar = gblank(sh.kdatrd  );if(kchar>mchar) kchar=mchar;
  strncpy(mchars[k] , sh.kdatrd, kchar);
    strcpy(mchars[k], tempchar);  k++;      

  kchar = gblank(sh.kinst);if(kchar>mchar) kchar=mchar;
  strncpy(tempchar , sh.kinst, kchar);
    strcpy(mchars[k], tempchar);  k++;      
    
    kchar = gblank(sh.kevnm);
  
  mchar = 16;
 if(kchar>mchar) kchar=mchar;
 strncpy(tempchar2 , sh.kevnm,  kchar );
  strcpy(echars[0], tempchar2);  k++;  


  /* echars[kchar] = '\0'; */       




  return(0);

}

/**********


make -f JMLmakefile drive_sacNhead
 

R CMD SHLIB get_sacNhead.o


**********/


/** FUNC DEF **/ void CALL_NEWSAC2R(char **fn1, double *x, double  *dubs, int *mints, char **mchars, char **echars )
{

  int i, j, numsamp;
  int kchar;

  SacHeader sh;
  float *sacd=NULL; /* sac data pointer */

  /*
    int mints[40];
    double dubs[70];
    char mchars[22][9]; 
    char echars[17];
  */


  read_sac(fn1[0], &sh, &sacd);

  
  numsamp = sh.npts;


#if DEBUG
   fprintf(stderr, "%d\n", numsamp); 
#endif
  
  i = SACHEAD2VEC(sh, dubs, mints , mchars, echars);

#if DEBUG
   fprintf(stderr, "%s   \n", fn1[0] ); 

 fprintf(stderr, "DUBS------------------------\n"); 

  for(i=0; i<70; i++)
    {
      j = i + 1;
      
      fprintf(stderr, "%f ", dubs[i]);
      if(j % 5== 0) 
	{

	  fprintf(stderr, "\n");


	}
	
    }
fprintf(stderr, "\n");

fprintf(stderr, "MINTS------------------------\n");

 for(i=0; i<40; i++)
    {
  j = i + 1;
      fprintf(stderr, "%d ", mints[i]);
      if(j % 5== 0) 
	{

	  fprintf(stderr, "\n");


	}

    }

fprintf(stderr, "\n");
fprintf(stderr, "MCHARS------------------------\n");

 for(i=0; i<22; i++)
    {
  j = i + 1;
      fprintf(stderr, "%s ", mchars[i]);
      if(j % 5== 0) 
	{

	  fprintf(stderr, "\n");


	}

    }

fprintf(stderr, "\n");

#endif



  
 for (i = 0; i < numsamp; i++) 
   {
     x[i] = sacd[i];
   }
  


}
/*****************/


/** FUNC DEF **/ void  get_one_sack(char *fn, SAC_SERIES *ts)
   {

   SacHeader sh;
   int i;
   int kchar;

   float *sacd=NULL; /* sac data pointer */


   read_sac(fn, &sh, &sacd);

   /* fprintf(stderr, "done reading in read_sac\n"); */
   
  
   ts->deltat = (float) sh.delta;
      ts->t1 = 0.0;
   ts->t2 = sh.e - sh.b;


  ts->numsamp = ((ts->t2 - ts->t1) / ts->deltat) + 1;


 /*  fprintf(stderr, "done reading in read_sac: %f %f %f %f\n",ts->t1,  ts->t2  , sh.e, sh.b    );  */

/*     fprintf(stderr, "done reading in read_sac: %f %d %d\n",ts->deltat,  ts->numsamp  , sh.npts    );  */

if( ts->numsamp != sh.npts)
   {
   ts->numsamp = sh.npts;
   ts->t1 = 0;
    ts->t2 = sh.npts*ts->deltat;
   }
   
   ts->year = sh.nzyear;
   ts->jday= sh.nzjday;
   
   ts->hour=(int)sh.nzhour;
   
   ts->minute=sh.nzmin;
   
   ts->sec=sh.nzsec+sh.nzmsec/1000.0;
   
    ts->epoch.year = ts->year;
     ts->epoch.julday = ts->jday;
     ts->epoch.hour =  ts->hour;
     ts->epoch.minute = ts->minute;
     ts->epoch.sec  = ts->sec;
     

    ts->scale_factor = sh.scale;

 
     ts->amp = (float *)calloc((unsigned)ts->numsamp , sizeof(float));  
     ts->ampbak = (float *)calloc((unsigned)ts->numsamp , sizeof(float));  

     ts->max =  sacd[0];
     ts->min =  sacd[0];

     ts->reference_time  = 0.0;


	/*************   units are now in volts ???  I am not sure ****************/


     for (i = 0; i < ts->numsamp; i++) 
      {
     /* if( (i%10000)==0) fprintf(stderr,"%d %d\n", i, a->dat[i]); */
      ts->amp[i] = sacd[i];
      ts->ampbak[i] = sacd[i];
      
      if( ts->amp[i]>ts->max) ts->max=sacd[i];
      if( ts->amp[i]<ts->min) ts->min=sacd[i];
      
      }

     if(ts->max==ts->min)
     {

	ts->min = ts->max-1;

     }


     kchar = gblank(sh.kstnm); if(kchar>8) kchar=8;

    /*   fprintf(stderr,"kstnm <%s> %d\n", sh.kstnm, kchar);  */
     strncpy(ts->id.staname,sh.kstnm , kchar);
     if(kchar>8) kchar=8;
     ts->id.staname[kchar] = '\0';  
     /*  fprintf(stderr,"new  <%s>\n", ts->id.staname);  */

     kchar = gblank(sh.kcmpnm);
       /* fprintf(stderr,"kcmpnm <%s> %d\n", sh.kcmpnm, kchar);  */
       if(kchar>8) kchar=8;
     strncpy(ts->id.comp ,sh.kcmpnm , kchar);
     ts->id.comp[kchar] = '\0';
       /* fprintf(stderr,"new  <%s>\n", ts->id.comp);  */

     ts->evla= sh.evla;
     ts->evlo= sh.evlo ;  
     ts->stla= sh.stla;   
     ts->stlo= sh.stlo ;  
     ts->stel= sh.stel;   
     ts->evel= sh.evel ;  

     ts->stel= sh.stdp;   
     ts->evel= sh.evdp ;  



     ts->ppick= sh.a;   
     ts->spick= sh.t0;
     ts->otime= sh.o;

     strcpy(ts->id.filename, fn);
     strcpy(ts->id.id_string, fn);
     /* fprintf(stderr, "done reading in read_sac: min=%f max=%f\n",ts->min, ts->max    ); */ 


   }
/***************done with sac******************/


/** FUNC DEF */ void CALL_GETSAC(char **fn, double *x, int *n, double *dt, int *DATE1, double  *SACINFO, double *sec)
{
 
  int i;

  SAC_SERIES ts;
  char *te;

  /* strcpy(te,fn[0]); */

  /* fprintf(stderr, "%s\n", fn[0]); */

   
 
  get_one_sack(fn[0],  &ts);

  *n =  ts.numsamp;
  *dt = ts.deltat;
  /* x=(double  *)malloc((size_t) ((ts.numsamp)*sizeof(double))); */

   
  for(i=0; i<ts.numsamp; i++)
    {
      x[i] = ts.amp[i];
    }

  DATE1[0] = ts.year;
  DATE1[1] = ts.jday;
  DATE1[2] = ts.hour;
  DATE1[3] = ts.minute;

  SACINFO[0] = ts.evla ;

 SACINFO[1] =ts.evlo; 

 SACINFO[2] =ts.evel;  
SACINFO[3] =ts.evdp; 

SACINFO[4] =ts.stla;  
SACINFO[5] =ts.stlo;  

SACINFO[6] =ts.stel;  
SACINFO[7] =ts.stdp; 


SACINFO[8] =ts.ppick;  
SACINFO[9] =ts.spick;  
SACINFO[10] =ts.otime; 





  *sec = ts.sec;


  free(ts.amp);
  free(ts.ampbak);

 
}
/** FUNC DEF */ void CALL_SETSAC(char **fn, int *n, double *dt, int *DATE1, double  *SACINFO, double *sec, char **stn, char **cmpn)
{
 
   int i;

   SAC_SERIES ts;
   char *te;

   /* strcpy(te,fn[0]); */

   /* fprintf(stderr, "%s\n", fn[0]); */

   
 
	 get_one_sack(fn[0],  &ts);
	 
   *n =  ts.numsamp;
   *dt = ts.deltat;

   DATE1[0] = ts.year;
   DATE1[1] = ts.jday;
   DATE1[2] = ts.hour;
   DATE1[3] = ts.minute;




 SACINFO[0] = ts.evla ;

 SACINFO[1] =ts.evlo; 

 SACINFO[2] =ts.evel;  
SACINFO[3] =ts.evdp; 

SACINFO[4] =ts.stla;  
SACINFO[5] =ts.stlo;  

SACINFO[6] =ts.stel;  
SACINFO[7] =ts.stdp; 


SACINFO[8] =ts.ppick;  
SACINFO[9] =ts.spick;  
SACINFO[10] =ts.otime; 



   *sec = ts.sec;

   /* fprintf(stderr, "station: %s\ncomp: %s\n", ts.id.staname, ts.id.comp); */

   strcpy(stn[0], ts.id.staname);
   strcpy(cmpn[0], ts.id.comp);

   free(ts.amp);
   free(ts.ampbak);

 
}





