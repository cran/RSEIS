#include <R_ext/Rdynload.h>
#include <R.h>
#include <Rinternals.h>
#include <R_ext/RS.h>
#include <stdlib.h> // for NULL


void  CALL_slepian(int *anpoints,int *inwin, double *dnpi, double *tapers);
void   CALL_WLETXCOR(double *LET, int *Nlet, double *sig, int *Nsig, double *xcor);
void CALL_JPIKI(int *epik, int *polar, int *pikwt, int x[], int *ix, int *itar, int  *IND);
 void  CALL_DFBRAT(double *seis, double  *fbcurv, int *npts,
		   int *fwlen, int *bwlen, int *logflg);
 void  CALL_MFBRAT(double *seis, double  *fbcurv, int *npts,
		   int *fwlen, int *bwlen, int *logflg);
void   CALL_DTTmany(int *n, double *delta, double *hpz, double *staz, int *nlay, double *ztop, double  *vel,
	     double *dtdr, double *dtdz, double *angle, double *outt);

void   CALL_DTT1(double *delta, double *hpz, double *staz, int *nlay, double *ztop, double  *vel,
		 double *dtdr, double *dtdz, double *angle, double *outt);

void   CALL_DTTray(double *delta, double *hpz, double *staz, int *nlay, double *ztop, double  *vel,
		   double *dtdr, double *dtdz, double *angle, double *outt, int *nnod, double *znod, double *rnod);

void  CALL_Mspec(double *data, int *anpoints, int *akind,
				int *anwin, double *anpi, int *ainorm, double *adt, 
				double *ospec, double *dof, 
		  double *Fvalues, int *aklen, double  *ReSpec, double   *ImSpec);

/* void  CALL_FINDWFREQ(double *data, int *anpoints, double *adt, int   *JOUT); */

void CALL_JFILT(double *input, int *na, int *iord, char **type, char **aproto,
		double *aa, double *atrbndw, double *afl, double *afh, double *ats,int *RemM, int *zerop, double  *output);

int  CALL_DLINE(double *x, double *y, int *length, double *slope, double *intercept);

 void CALL_DCORN(double *x, double *y, int  *num_freqs, 
	      int  *K, double *final_ave,
		 double *final_slope, double *final_intercept);
 void CALL_DGAMMA(double *x, double *y, int *num_freqs, 
				    double *fcorn, double *omega,
				   double dgam[3], int *ngam,
		  double dstar[3], int *nstar);

void   CALL_ARAIC(double *y1, int *inum, double *deltat, int *p, 
		  int *iT1, double *iO1, double *iO2, double *iW, double *kout);

void   CALL_ARspec(double *y1, double *kout, int *inum,  int *p,  int *numc);

static const  R_CMethodDef CEntries[] = {
        {"CALL_slepian", (DL_FUNC) &CALL_slepian, 4},
{"CALL_WLETXCOR", (DL_FUNC) &CALL_WLETXCOR, 5},
{"CALL_JPIKI", (DL_FUNC) &CALL_JPIKI , 7 },
{"CALL_DFBRAT", (DL_FUNC) &CALL_DFBRAT ,  6},
{"CALL_MFBRAT", (DL_FUNC) &CALL_MFBRAT, 6 },
{"CALL_DTTmany", (DL_FUNC) &CALL_DTTmany, 11 },
{"CALL_DTT1", (DL_FUNC) &CALL_DTT1,  10},
{"CALL_DTTray", (DL_FUNC) &CALL_DTTray,  13},
 {"CALL_Mspec", (DL_FUNC) &CALL_Mspec,  13},
	/* {"CALL_FINDWFREQ", (DL_FUNC) &CALL_FINDWFREQ, 4 }, */
{"CALL_JFILT", (DL_FUNC) &CALL_JFILT,  13},
{"CALL_DLINE", (DL_FUNC) &CALL_DLINE,  5},
{"CALL_DCORN", (DL_FUNC) &CALL_DCORN,  7},
{"CALL_DGAMMA", (DL_FUNC) &CALL_DGAMMA,  9},
{"CALL_ARAIC", (DL_FUNC) &CALL_ARAIC, 9 },
{"CALL_ARspec", (DL_FUNC) &CALL_ARspec,  5},
        {NULL, NULL, 0}
     };

   void
     R_init_RSEIS(DllInfo *dll)
     {
        R_registerRoutines(dll, CEntries, NULL, NULL, NULL);
        R_useDynamicSymbols(dll, FALSE);  
     }
