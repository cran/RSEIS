
#include "segy.h"
#include "defines.h"

#define XtraceNewArray(T, N) \
        (T *)calloc((size_t)(N), sizeof(T))
 
#define XtraceNewElement(T) \
        XtraceNewArray(T, 1)
 
#define XtraceNewString(N) \
        XtraceNewArray(char, N)
 
#define XtraceFree(P) \
        free((void *)(P))
 

typedef struct DateTime {
    int year, mon, day, hour, minute, julday;
    double sec;
}  DateTime; /* absolute Date and Time*/
 


typedef struct _SEGYTrace 
   {
   struct SegyHead hd;
   int *dat;
   } SEGYTrace;


typedef struct PICK
{
  DateTime time;
  char name[10];
} PICK;  /*   pick time for storage  */

typedef struct 
   {

      int year, month, day, hour, minute, second, m_secs;
      int julday;
      float latsrc, lonsrc, latsta, lonsta;
      float depsrc, depsta;
      int channel;
      char id_string[100];
      
      char staname[7], comp[7];
      char filename[100];
      char dirname[100];
      char sensor_type[10];

   } TraceID;


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
      
} TIME_SERIES ;
