#ifndef DEFINE_INCLUDE
#define DEFINE_INCLUDE

#define VER2p1
#define	 VER_NO "97.033"
/* #define PQL_VER_STRING "PASSCAL Quick Look 2.1, Rev VER_NO" */

 
/* defines for logical data types */
/* #define TRUE    1
   #define FALSE   0
   */

#define SOCKET_NAME "pql_socket"
/* #define SOCKET_NUMBER 9876 use a global int instead */
#define NUM_LISTS       7  /* the number of lists to sort by */

#define SUCCESS 0
#define FAILURE -1


#define DRIP_FILE_NAME "/usr/demo/SOUND/sounds/drip.au"
/* used as a sound effect */

#define MIN_MAIN_WIDTH  900 
#define MIN_MAG_WIDTH   1100   /* the minimum width allowed before */ 
#define MIN_SPEC_WIDTH  1000   /* the buttons must be wrapped */
#define SPEC_LEFT_BUTTONS 315  /* the distance from the left edge to */
                               /* the right edge of the filter:on/off */
			       /* label, to be used to calculate button */
			       /* position  */
#define SPEC_RIGHT_BUTTONS 450  /* the width of the left side buttons */

#define MIN_NUM_POINTS_W_NOMAG 20000   /* assume some lower limit for */
				       /* deciding if superzoom is allowed */ 
#define MIN_NUM_POINTS_SUPER 10

#define R_SHIFT 32550
#define L_SHIFT 32549


#define SEGY_DATA	0
#define AH_DATA		1
#define SAC_DATA	2
#define MSEED_DATA	3
#define NANO_DATA	4
#define SUDS_DATA       5
#define STEIM1_DATA	6
#define STEIM2_DATA	7


#define ABSOLUTE	0
#define RELATIVE	1

/*  #define CHECK_BOXES  */
/* uncomment the above if you desire the use of check boxes for */
/* volts/counts and trace_window selection in all windows */

#ifdef CHECK_BOXES
#define TRACE		1
#define WIND		2
#define VOLTS		1
#define COUNTS		2
#else
#define TRACE		0
#define WIND		1
#define VOLTS		0
#define COUNTS          1
#endif

/*
  */


#define LIN		0
#define LOG		1

#define HOUR            1
#define MINUTE          2
#define SECOND          3
#define SEC_FRAC        4

/* #define MAXPLOTS	30 */
#define MAXPLOTS	100
#define MAXFILES	2000

#define MAXFREQ         10000
#define MINFREQ         1
#define MAXPOLES        12

#define BIT32	1
#define BIT16	0
#define MY_FLOAT 2

#define MAIN_MARGIN	25
#define MAG_T_MARGIN	25
#define MAG_B_MARGIN	35

#define PRINT_FILE      0
#define PRINT_W_USER    1
#define PRINT_PRINTER   2

#define LABEL_FILE      0
#define LABEL_HEADER    1



/* new defines by Song*/
#define	 SHOW_TIME_NONE	0
#define	 SHOW_TIME_P	1
#define	 SHOW_TIME_S	2
#define	 SHOW_TIME_Pplus	3
#define	 SHOW_TIME_Splus	4
#define	 SHOW_TIME_P_and_S		5
#define	 SHOW_TIME_Pplus_and_Splus	6
#define	 SHOW_TIME_BASIC	7
#define	 SHOW_TIME_ALL	8

#define MAX_TT          60 /* max number of ttime phases*/

typedef struct t_time {
	int event_ready; 		/* set to 1 if event infor read */
	float depth; 			/* the depth of the source */
	float delta; 			/* the arc distance of source receiver */
        char phase_group[10][8];         /* available phases groups */
        int n;                  /* number of phase for a perticular depth and delta*/ 
	char phase[MAX_TT][8]; 	/* phase names 8 in FORTRAN 11/03/93 wykim */
	float tt[MAX_TT]; 		/* travel time in seconds */
        float dtdd[MAX_TT];             /* dt/dd=p ray horizontal slowness */
        float dtdh[MAX_TT];             /* dt/dh */
        float dddp[MAX_TT];             /* dd/dp =d-delta/dp */
	int   p_or_s[MAX_TT]; 		/* =1 if a S end phase, =0 a P end phase */
	int ready;
} t_time;


#define	FR	fprintf(stderr

#endif
