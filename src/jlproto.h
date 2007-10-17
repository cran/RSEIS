



int  multitap(int num_points, int nwin, double *lam, double npi, double *tapers, double *tapsum);
int hires(double *sqr_spec,  double *el, int nwin, int num_freq, double *ares);
void  get_F_values(double *sr, double *si, int nf, int nwin, double *Fvalue, double *b);
 int adwait(double *sqr_spec,  double *dcf,
            double *el, int nwin, int num_freq, double *ares, double *degf, double avar);
int  multitap(int num_points, int nwin, double *lam, double npi, double *tapers, double *tapsum);
void  mt_get_spec(double *series, int inum, int klength, double *amp);



void jmsg(char *a);

float    *fjector(long nl, long nh);
double   *djector(long nl, long nh);
int      *ijector(long nl, long nh);

void  free_fjector(float *v, long nl, long nh);
void  free_ijector(int *v, long nl, long nh);
void  free_djector(double *v, long nl, long nh);


void  zero_pad(double output[], int start , int olength);
void  dfour1(double  data[], unsigned long nn, int isign);
void  jrealft(double data[], unsigned long n, int isign);

 void do_mtap_spec(double *data, int npoints, int kind,
		   int nwin, double npi, int inorm, double dt, double *ospec, double *dof, double *Fvalues, int klen);
void  Mtap_spec(double *data, int npoints, int kind,
	    int nwin, double npi, int inorm, double dt, double *ospec, double *dof, 
		double *Fvalues, int klen, double  *ReSpec, double   *ImSpec);
