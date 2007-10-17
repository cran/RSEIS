#include <stdio.h>

#include <assert.h>
#include <math.h>
#include <string.h>
#include <time.h>
#include <stdlib.h>
#include <unistd.h>
#include <stddef.h>


#include <malloc.h>

/**********   change 	USE_AH_XDR  to 0 to exclude all calls to XDR (AH)   **/
/*                           MUST make sure this is correct   **/
/*                              for windows, set this to 0  **/
/**************************************************************************/
#define	USE_AH_XDR 0
/**************************************************************************/

/*	low level i/o routines for ah format records
 *		-- witte	6 june 85
 */


#if USE_AH_XDR
#include <rpc/rpc.h>
#include <sys/ioctl.h>
#include <sys/stat.h>

#endif

#include "seis.h"

#include "sac.h"
char	*progname;


#if USE_AH_XDR

#include "ahhead.h"

int get_null_head(ahhed	*hed);
int xdr_ahhead(XDR *xdrsp, ahhed *ahheadp);

/* ah error processing */

int	ah_errno = 0;
int	ah_nerr = 10;

/* ah error numbers */
#define	AE_RHED		1	/* error reading header	*/
#define	AE_DTYPE	2	/* bad data type	*/
#define	AE_WHED		3	/* error writing header	*/
#define	AE_RDATA	4	/* error reading data	*/
#define	AE_WDATA	5	/* error writing data	*/
#define	AE_WRECORD	6	/* error writing record	*/
#define	AE_RRECORD	7	/* error reading record	*/
#define	AE_TTYOUT	8	/* binary going to tty	*/
#define	AE_TTYIN	9	/* binary coming from tty	*/

/* ah errlist */

char	*ah_errlist[] = {
		"no error",		/* 0	no error	*/
		"read header error",	/* 1	AE_RHED		*/
		"bad data type",	/* 2	AE_DTYPE	*/
		"write header error",	/* 3	AE_WHED		*/
		"read data error",	/* 4	AE_RDATA	*/
		"write data error",	/* 5	AE_WDATA	*/
		"write record error",	/* 6	AE_WRECORD	*/
		"read record error",	/* 7	AE_RRECORD	*/
		"tty can't get binary",	/* 8	AE_TTYOUT	*/
		"tty can't send binary"	/* 9	AE_TTYIN	*/
			};


/*	gethead
 *		gets the next header from the stream pointed to by
 *		file_pt and returns this header in the structure head.
 *		file_pt is assumed to be positioned at the next header,
 *		and does not search.
 *	returns:
 *			1		->	no error
 *			-1		->	not enough head to read
 *			-2		->	bad data type
 */
int	gethead(head,file_pt)
	FILE	*file_pt;
	ahhed	*head;
{
	int	ierr = 0;

	if((ierr = fread((char *)head,sizeof(ahhed),1,file_pt)) == 1)
	{
		if((head->record.type < TYPEMIN) || (head->record.type > TYPEMAX))
		{
			get_null_head(head);
			ierr = -2;	/* bad data type */
			ah_errno= AE_DTYPE;
		}
	}
	else		/* not enough head */
	{
		get_null_head(head);
		ierr = -1;
		ah_errno= AE_RHED;
	}
	return(ierr);
}



/*	ahputhead
 *		writes the header head onto the stream pointed to by
 *		file_pt.
 *	returns:
 *			1		->	no error
 *			-1		->	error writing header
 */
int	ahputhead(head,file_pt)
	FILE	*file_pt;
	ahhed	*head;
{
	int	ierr = 0;

	if((ierr= fwrite((char *)head,sizeof(ahhed),1,file_pt)) != 1)
	{
		ah_errno= AE_WHED;
		ierr= -1;
	}
	return(ierr);
}



/*	size
 *		returns the size (in bytes) of the data type given by
 *		head->record.type.
 *	returns:
 *			size of data type	->	no error
 *			-1			->	unknown data type
 */
int	size(head)
	ahhed	*head;
{
	int	type_size = 0;

	switch(head->record.type)
	{
	case 1:		/* real time series */
		type_size= sizeof(float);
		break;
	case 2:		/* complex time series */
		type_size= sizeof(complex);
		break;
	case 3:		/* real x,y pairs */
		type_size= sizeof(vector);
		break;
	case 4:		/* x real, y complex, or real x,y,z */
		type_size= sizeof(tensor);
		break;
	case 5:		/* complex x,y pairs */
		type_size= 2*sizeof(complex);
		break;
	case 6:		/* double */
		type_size=sizeof(double);
		break;
	default:	/* unknown data type */
		type_size= -1;
		ah_errno= AE_DTYPE;
		break;
	}
	return(type_size);
}


/*	tohead
 *		positions the read/write head to the beginning of the
 *		n-th header in the file pointed to by file_pt.
 *	returns:
 *			n	->	no error
 *			-1	->	not enough heads
 *			-2	->	bad seek
 */
int	tohead(n,file_pt)
	FILE	*file_pt;
	int	n;
{
	ahhed	head;
	int	i,ierr;

	rewind(file_pt);
	for(i=1; i<n; ++i)
	{
		if(gethead(&head,file_pt) == 1)
		{
			if(fseek(file_pt,(long)(head.record.ndata)*(size(&head)),1) == -1)
			{
				ierr = -2;	/* bad seek */
				ah_errno= AE_RHED;
				return(ierr);
			}
		}
		else
		{
			ierr = -1;	/* not enough head */
			ah_errno= AE_RHED;
			return(ierr);
		}
	}
	return(i);	/* success */
}



/*	getdata
 *		reads from the file pointed to by file_pt into
 *		the array pointed to by array.  It assumes that
 *		the read/write head is positioned correctly 
 *		(i.e., right after the header), and does not
 *		search.  Works for any allowed data type.
 *	returns:
 *			number of elements read	->	OK
 *			-1			->	error
 */
int	getdata(head,array,file_pt)
	ahhed	*head;
	char	*array;
	FILE	*file_pt;

{
	int ierr = 0;

	if((ierr = fread(array,size(head),(int)head->record.ndata,file_pt)) != (int)head->record.ndata)
	{
		ah_errno= AE_RDATA;
		ierr = -1;
	}

	return(ierr);
}


/*	putdata
 *		writes array to the file pointed to by
 *		file_pt.  Works for any allowed data type.
 *	returns:
 *			number of elements written	->	OK
 *			-1			->	error
 */
int	putdata(head,array,file_pt)
	ahhed	*head;
	char	*array;
	FILE	*file_pt;
{
	int	ierr = 0;

	if((ierr = fwrite(array,size(head),(int)head->record.ndata,file_pt)) != (int)head->record.ndata)
	{
		ah_errno= AE_WDATA;
		ierr = -1;
	}
	return(ierr);
}


/*	putrecord
 *		writes head and array to the file pointed to by
 *		file_pt.  Works for any allowed data type.
 *	returns:
 *			0	->	OK
 *			-1	->	error writing header
 *			-2	->	error writing data
 */
int	putrecord(head,array,file_pt)
	ahhed	*head;
	char	*array;
	FILE	*file_pt;
{
	int	ierr = 0;

	(ahputhead(head,file_pt) == 1) ? ((putdata(head,array,file_pt) < 0) ? (ierr = -2) : (ierr = 0)) : (ierr = -1);
	if(ierr)
		ah_errno= AE_WRECORD;

	return(ierr);
}


/*	getrecord
 *		gets header and data from the file pointed to by
 *		file_pt and puts them in head and array.  It assumes
 *		that the read/write head is positioned at the beginning
 *		of the header, and does not search.  Obviously, calling
 *		routine must have allocated enough space.
 *	returns:
 *			0	->	OK
 *			-1	->	error reading header
 *			-2	->	error reading data
 */
int	getrecord(head,array,file_pt)
	ahhed	*head;
	char	*array;
	FILE	*file_pt;
{
	int	ierr = 0;

	(gethead(head,file_pt) == 1) ? ((getdata(head,array,file_pt) < 0) ? (ierr = -2) : (ierr = 0)) : (ierr = -1);
	if(ierr)
		ah_errno= AE_RRECORD;
	return(ierr);
}

/*
 *	getrecord2
 *		gets header and data from the file pointed to by
 *		file_pt and puts them in head and array.  It assumes
 *		that the read/write head is positioned at the beginning
 *		of the header, and does not search (although it does
 *		some error checking).  Space for array is allocated, so
 *		be sure to pass a pointer to the data pointer. Got it?
 *	returns:
 *			0	->	ok
 *			-1	->	error reading record
 *			-2	->	error allocating space for data
 */
int	getrecord2(head,array,file_pt)
	ahhed	*head;
	char	**array;
	FILE	*file_pt;
{
	int	ierr = 0;
	int	gethead();
	char	*mkdatspace();

	if(gethead(head, file_pt) != 1) {
		ierr = -1;
		return(ierr);
	}

	*array= mkdatspace(head);
	if(*array == NULL) {
		ierr= -2;
		return(ierr);
	}

	if(getdata(head,*array,file_pt) < 0)
		ierr= -1;

	return(ierr);
}


/*	gogethead
 *		gets n-th header from the stream pointed to by
 *		file_pt and returns this header in the structure
 *		head.
 *	returns:
 *			0	->	OK
 *			-1	->	stream not long enough
 *			-2	->	error reading header
 */
int	gogethead(n,head,file_pt)
	int	n;
	ahhed	*head;
	FILE	*file_pt;
{
	int	ierr = 0;

	(tohead(n,file_pt) == n) ? ((gethead(head,file_pt) < 1) ? (ierr = -2) : (ierr = 0)) : (ierr = -1);
	return(ierr);
}


/*	gogetrecord
 *		gets n-th record (header and data) from the stream
 *		pointed to by file_pt and places it in head and array.
 *		Calling routine must allocate enough space.
 *	returns:
 *			0	->	OK
 *			-1	->	stream not long enough
 *			-2	->	error reading record
 */
int	gogetrecord(n,head,array,file_pt)
	int	n;
	ahhed	*head;
	char	*array;
	FILE	*file_pt;

{
	int	ierr = 0;

	(tohead(n,file_pt) == n) ? ((getrecord(head,array,file_pt) < 0) ? (ierr = -2) : (ierr = 0)) : (ierr = -1);
	return(ierr);
}

/* logger adds a 10 character comment to the log section of the header
 * comment should be passed as a character pointer must be terminated
 * by a ';' and a `/0`
 * returns:
 *	logger =  0  -> log info added to header structure
 *	logger = -1  -> no ';', added
 *	logger = -2  -> input string greater than LOGENT
 *                      input truncated to allowable limit
 *	logger = -3  -> attempt to make log string greater than LOGSIZE
 *                      input comment truncated to fit 
 *
 *			written by Tom Boyd   6/10/85
 */

int logger(char_pt,head_pt)
ahhed *head_pt;
char *char_pt;

{
	int org,in,err,diff;
	//  char *strncat();

	err=0;

/* find length of log array and input array  */

	org=strlen(head_pt->record.log);  /*log array*/
	in=strlen(char_pt);  /*input array*/

/* check for a terminating ':' in the input array */

	if(*(char_pt+in-1) != ';')
	{                   /* no semicolon----add it*/
		err=(-1);
		*(char_pt+in)=';';
		*(char_pt+in+1)='\0';
		in+=1;
	}

/* check the length of the input array */

	if(in > LOGENT)
	{                  /* entry length too long-----truncate it*/
		err=(-2);
		*(char_pt+LOGENT-1)=';';
		*(char_pt+LOGENT)='\0';
		in=LOGENT;
	}

/* check combined length of array and new input and add it */

	diff=LOGSIZE-(org+in);
	if(diff == -in) return(-3);  /* no room left in log array */
	if(diff < 0)diff*=(-1),err=(-3);  /*partial room left----use it */	
	strncat(head_pt->record.log,char_pt,diff); /* cat two strings */

	return(err);
}



/*	out_is_tty
 *		determines whether stdout is being sent to screen.
 *	returns:
 *			0	->	stdout is not tty
 *			1	->	stdout is tty
 */
int	out_is_tty()

{

	if(isatty(1))	/* sun specific --- stdout */
	{
		ah_errno= AE_TTYOUT;
		return(1);
	}
	return(0);
}


/*	in_is_tty
 *		determines whether stdin is tty
 *	returns:
 *			0	->	stdin is not tty
 *			1	->	stdin is tty
 */
int	in_is_tty()
{

	if(isatty(0))	/* sun specific --- stdin */
	{
		ah_errno= AE_TTYIN;
		return(1);
	}
	return(0);
}


/*	mkdatspace
 *		allocates enough space for the data array, and
 *		returns a pointer to the memory location, or
 *		NULL if failure.
 *	returns:
 *			character pointer	->	success
 *			NULL			->	failure
 */
char	*mkdatspace(head)
	ahhed	*head;
{
	/* char *calloc(); */
	
	return(calloc((unsigned)head->record.ndata,(unsigned)size(head)));
}



int get_null_head(ahhed	*hed)
{
int	i;
char	*strcpy();

	strcpy(hed->station.code,"null");
	strcpy(hed->station.chan,"null");
	strcpy(hed->station.stype,"null");
	hed->station.slat= 0.0;
	hed->station.slon= 0.0;
	hed->station.elev= 0.0;
	hed->station.DS= 0.0;
	hed->station.A0= 0.0;
	for(i=0; i< NOCALPTS; ++i)
	{
		hed->station.cal[i].pole.r= 0.0;
		hed->station.cal[i].pole.i= 0.0;
		hed->station.cal[i].zero.r= 0.0;
		hed->station.cal[i].zero.i= 0.0;
	}

	hed->event.lat= 0.0;
	hed->event.lon= 0.0;
	hed->event.dep= 0.0;
	hed->event.ot.yr= (short)0;
	hed->event.ot.mo= (short)0;
	hed->event.ot.day= (short)0;
	hed->event.ot.hr= (short)0;
	hed->event.ot.mn= (short)0;
	hed->event.ot.sec= 0.0;
	strcpy(hed->event.ecomment,"null");

	hed->record.type= (short)0;
	hed->record.ndata= 0L;
	hed->record.delta= 0.0;
	hed->record.maxamp= 0.0;
	hed->record.abstime.yr= (short)0;
	hed->record.abstime.mo= (short)0;
	hed->record.abstime.day= (short)0;
	hed->record.abstime.hr= (short)0;
	hed->record.abstime.mn= (short)0;
	hed->record.abstime.sec= 0.0;
	hed->record.rmin= 0.0;
	strcpy(hed->record.rcomment,"null");
	strcpy(hed->record.log,"null");

	for(i=0; i< NEXTRAS; ++i)
		hed->extra[i]= 0.0;

	return(1);
}

/* acpy(from,to,nbytes) copies nbytes from the array "from" to the
 *	array "to".
 */
int acpy(char	*from,char	*to,unsigned nbytes)
{
	while(nbytes--)
		*from++ = *to++;
	return(0);
}


int ah_error(char	*s1,char	*s2,int status)		/* print ah format error message and die */
{
	extern	char	*progname;

	if(progname)
		fprintf(stderr,"%s: ",progname);
	fprintf(stderr,s1,s2);
	if(ah_errno > 0 && ah_errno < ah_nerr)
		fprintf(stderr," (%s)",ah_errlist[ah_errno]);
	fprintf(stderr,"\n");
	exit(status);
}

#define	MAX(a,b)	(((a) > (b)) ? (a) : (b))
#define	MIN(a,b)	(((a) < (b)) ? (a) : (b))

/*
 *	maxamp
 *		determines the maximum absolute amplitude of the data array, and
 *		places that number in head.record.maxamp.
 *	returns:
 *			0	->	ok
 *			-1	->	error
 */
int	maxamp(head,data)
 	ahhed	*head;
 	char	*data;
{
	float	*fpt;
	double *dpt, dmin, dmax;
	float	max,min;
	long	n_data_pts;

	switch(head->record.type)
	{
	case FLOAT:
		n_data_pts= head->record.ndata;
		break;
	case COMPLEX:
	case VECTOR:
		n_data_pts= 2 * head->record.ndata;
		break;
	case TENSOR:
		n_data_pts= 3 * head->record.ndata;
		break;
	case 5:
		n_data_pts= 4 * head->record.ndata;
		break;
	case DOUBLE:
		n_data_pts= head->record.ndata;
		break;
	default:
		ah_errno= AE_DTYPE;
		return(-1);
		break;
	}

	if (head->record.type == DOUBLE) {
		dpt= (double *)data;
		dmax= dmin= *dpt;
		while(n_data_pts--)
		{
			dmax= MAX(dmax,*dpt);
			dmin= MIN(dmin,*dpt);
			++dpt;
		}
		((fabs(dmax) > fabs(dmin)) ? (head->record.maxamp= (float) dmax) : (head->record.maxamp= (float) -dmin));
	}
	else {
		fpt= (float *)data;
		max= min= *fpt;
		while(n_data_pts--)
		{
			max= MAX(max,*fpt);
			min= MIN(min,*fpt);
			++fpt;
		}
		((fabs((double)max) > fabs((double)min)) ? (head->record.maxamp= max) : (head->record.maxamp= -min));
	}

	return(0);
}
/*	xdr_gethead
 *		gets the next header from the xdr stream pointed to by
 *		xdrs and returns this header in the structure head.
 *		xdrs is assumed to be positioned at the next header,
 *		and does not search.
 *	returns:
 *			1		->	no error
 *			-1		->	not enough head to read
 *			-2		->	bad data type
 */
int	xdr_gethead(head,xdrs)
	XDR	*xdrs;
	ahhed	*head;
{
	int	ierr = 0;

	if((ierr = xdr_ahhead(xdrs, head)) == 1)
	{
		if((head->record.type < TYPEMIN) || (head->record.type > TYPEMAX))
		{
			get_null_head(head);
			ierr = -2;	/* bad data type */
			ah_errno= AE_DTYPE;
		}
	}
	else		/* not enough head */
	{
		get_null_head(head);
		ierr = -1;
		ah_errno= AE_RHED;
	}
	return(ierr);
}



/*	xdr_puthead
 *		writes the header head onto the xdr stream pointed to by
 *		xdrs.
 *	returns:
 *			1		->	no error
 *			-1		->	error writing header
 */
int	xdr_puthead(head,xdrs)
	XDR	*xdrs;
	ahhed	*head;
{
	int	ierr = 0;

	if((ierr= xdr_ahhead(xdrs, head)) != 1)
	{
		ah_errno= AE_WHED;
		ierr= -1;
	}
	return(ierr);
}



/*	xdr_tohead
 *		positions the read/write head to the beginning of the
 *		n-th header in the xdr stream pointed to by xdrs.
 *	returns:
 *			n	->	no error
 *			-1	->	not enough heads
 *			-2	->	bad seek
 */
int	xdr_tohead(n,xdrs)
	XDR	*xdrs;
	int	n;
{
	ahhed	head;
	int	i,ierr,j;
	float	float_dum;
	double	double_dum;
	complex complex_dum;
	tensor	tensor_dum;

/* be warned: the following xdr_setpos call may not work at all 	*/
/* depending on the stream.  The use of 0 to get to the beginning 	*/
/* works empirically, but is not documented  ... sigh	- dws		*/
	xdr_setpos(xdrs, (u_int) 0);

	for(i=1; i<n; ++i)
	{
		if(xdr_gethead(&head,xdrs) == 1)
		{
			switch (head.record.type) {
			case FLOAT:
				for (j = 0; j < head.record.ndata; j++) {
					if (!xdr_float(xdrs, &float_dum)) {
						ierr = -2;	/* bad seek */
						ah_errno= AE_RHED;
						return(ierr);
					}
				}

				break;
			case COMPLEX:
			case VECTOR:
				for (j = 0; j < head.record.ndata; j++) {
					if (!xdr_float(xdrs, &complex_dum.i) ||
					    !xdr_float(xdrs, &complex_dum.r)) {
						ierr = -2;	/* bad seek */
						ah_errno= AE_RHED;
						return(ierr);
					}
				}

				break;
			case TENSOR:
				for (j = 0; j < head.record.ndata; j++) {
					if (!xdr_float(xdrs, &tensor_dum.xx) ||
					    !xdr_float(xdrs, &tensor_dum.yy) ||
					    !xdr_float(xdrs, &tensor_dum.xy)) {
						ierr = -2;	/* bad seek */
						ah_errno= AE_RHED;
						return(ierr);
					}
				}
				break;
			case 5:
				for (j = 0; j < 4 * head.record.ndata; j++) {
					if (!xdr_float(xdrs, &float_dum)) {
						ierr = -2;	/* bad seek */
						ah_errno= AE_RHED;
						return(ierr);
					}
				}
				break;
			case DOUBLE:
				for (j = 0; j < head.record.ndata; j++) {
					if (!xdr_double(xdrs, &double_dum)) {
						ierr = -2;	/* bad seek */
						ah_errno= AE_RHED;
						return(ierr);
					}
				}
				break;
			default:
				ierr = -2;	/* bad seek */
				ah_errno= AE_DTYPE;
				return(ierr);
			}
		}
		else
		{
			ierr = -1;	/* not enough head */
			ah_errno= AE_RHED;
			return(ierr);
		}
	}
	return(i);	/* success */
}



/*	xdr_getdata
 *		reads from the xdr stream pointed to by xdrs into
 *		the array pointed to by array.  It assumes that
 *		the read/write head is positioned correctly 
 *		(i.e., right after the header), and does not
 *		search.  Works for any allowed data type.
 *	returns:
 *			number of elements read	->	OK
 *			-1			->	error
 */
int	xdr_getdata(head,array,xdrs)
	ahhed	*head;
	char	*array;
	XDR	*xdrs;

{
	int ierr = 0;
	float *pfloat;
	double	*pdouble;
	complex *pcomplex;
	tensor	*ptensor;
	int i;

	switch(head->record.type) {
	case FLOAT:
		pfloat = (float *) array;
		for (i = 0; i < head->record.ndata; i++) {
			if (! xdr_float(xdrs, pfloat++) ) {
				ah_errno= AE_RDATA;
				ierr = -1;
				return(ierr);
			}
			++ierr;
		}
		break;
	case COMPLEX:
	case VECTOR:
		pcomplex = (complex *) array;
		for (i = 0; i < head->record.ndata; i++) {
			if (!xdr_float(xdrs, &(pcomplex->r)) ||
			    !xdr_float(xdrs, &(pcomplex++->i))) {
				ah_errno= AE_RDATA;
				ierr = -1;
				return(ierr);
			}
			++ierr;
		}
		break;
	case TENSOR:
		ptensor = (tensor *) array;
		for (i = 0; i < head->record.ndata; i++) {
			if (!xdr_float(xdrs, &(ptensor->xx)) ||
			    !xdr_float(xdrs, &(ptensor->yy)) ||
			    !xdr_float(xdrs, &(ptensor++->xy))) {
				ah_errno= AE_RDATA;
				ierr = -1;
				return(ierr);
			}
			++ierr;
		}
		break;
	case 5:
		pfloat = (float *) array;
		for (i = 0; i < 4 * head->record.ndata; i++) {
			if (! xdr_float(xdrs, pfloat++) ) {
				ah_errno= AE_RDATA;
				ierr = -1;
				return(ierr);
			}
			++ierr;
		}
		break;
	case DOUBLE:
		pdouble = (double *) array;
		for (i = 0; i < head->record.ndata; i++) {
			if (! xdr_double(xdrs, pdouble++) ) {
				ah_errno= AE_RDATA;
				ierr = -1;
				return(ierr);
			}
			++ierr;
		}
		break;
	default:
		ierr = -1;
		ah_errno= AE_DTYPE;
		return(ierr);
	}
	return(ierr);
}


/*	xdr_putdata
 *		writes array to the xdr stream pointed to by xdrs.
 *		Works for any allowed data type.
 *	returns:
 *			number of elements written	->	OK
 *			-1			->	error
 */
int	xdr_putdata(head,array,xdrs)
	ahhed	*head;
	char	*array;
	XDR	*xdrs;
{
	int	ierr = 0;
	float *pfloat;
	double	*pdouble;
	complex *pcomplex;
	tensor	*ptensor;
	int *pint;
	int i;

	switch(head->record.type) {
	case FLOAT:
		pfloat = (float *) array;
		for (i = 0; i < head->record.ndata; i++) {
			if (! xdr_float(xdrs, pfloat++) ) {
				ah_errno= AE_RDATA;
				ierr = -1;
				return(ierr);
			}
			++ierr;
		}
		break;
	case SHORTINT:
		pint = (int *) array;
		for (i = 0; i < head->record.ndata/2; i++) {
			if( ! xdr_int(xdrs,pint++) ) {
				ah_errno= AE_RDATA;
				ierr = -1;
				return(ierr);
			}
			++ierr;
		}
		break;
	case COMPLEX:
	case VECTOR:
		pcomplex = (complex *) array;
		for (i = 0; i < head->record.ndata; i++) {
			if (!xdr_float(xdrs, &(pcomplex->r)) ||
			    !xdr_float(xdrs, &(pcomplex++->i))) {
				ah_errno= AE_RDATA;
				ierr = -1;
				return(ierr);
			}
			++ierr;
		}
		break;
	case TENSOR:
		ptensor = (tensor *) array;
		for (i = 0; i < head->record.ndata; i++) {
			if (!xdr_float(xdrs, &(ptensor->xx)) ||
			    !xdr_float(xdrs, &(ptensor->yy)) ||
			    !xdr_float(xdrs, &(ptensor++->xy))) {
				ah_errno= AE_RDATA;
				ierr = -1;
				return(ierr);
			}
			++ierr;
		}
		break;
	case 5:
		pfloat = (float *) array;
		for (i = 0; i < 4 * head->record.ndata; i++) {
			if (! xdr_float(xdrs, pfloat++) ) {
				ah_errno= AE_RDATA;
				ierr = -1;
				return(ierr);
			}
			++ierr;
		}
		break;
	case DOUBLE:
		pdouble = (double *) array;
		for (i = 0; i < head->record.ndata; i++) {
			if (! xdr_double(xdrs, pdouble++) ) {
				ah_errno= AE_RDATA;
				ierr = -1;
				return(ierr);
			}
			++ierr;
		}
		break;

	default:
		ierr = -1;
		ah_errno= AE_DTYPE;
		return(ierr);
	}
	return(ierr);
}


/*	xdr_putrecord
 *		writes head and array to the xdr stream pointed to by xdrs.
 *		Works for any allowed data type.
 *	returns:
 *			0	->	OK
 *			-1	->	error writing header
 *			-2	->	error writing data
 */
int	xdr_putrecord(head,array,xdrs)
	ahhed	*head;
	char	*array;
	XDR	*xdrs;
{
	int	ierr = 0;

	(xdr_puthead(head,xdrs) == 1) ? ((xdr_putdata(head,array,xdrs) < 0) ? (ierr = -2) : (ierr = 0)) : (ierr = -1);
	if(ierr)
		ah_errno= AE_WRECORD;

	return(ierr);
}


/*	xdr_getrecord
 *		gets header and data from the xdr stream pointed to by
 *		xdrs and puts them in head and array.  It assumes
 *		that the read/write head is positioned at the beginning
 *		of the header, and does not search.  Obviously, calling
 *		routine must have allocated enough space.
 *	returns:
 *			0	->	OK
 *			-1	->	error reading header
 *			-2	->	error reading data
 */
int	xdr_getrecord(head,array,xdrs)
	ahhed	*head;
	char	*array;
	XDR	*xdrs;
{
	int	ierr = 0;

	(xdr_gethead(head,xdrs) == 1) ? ((xdr_getdata(head,array,xdrs) < 0) ? (ierr = -2) : (ierr = 0)) : (ierr = -1);
	if(ierr)
		ah_errno= AE_RRECORD;
	return(ierr);
}

/*
 *	xdr_getrecord2
 *		gets header and data from the xdr stream pointed to by
 *		xdrs and puts them in head and array.  It assumes
 *		that the read/write head is positioned at the beginning
 *		of the header, and does not search (although it does
 *		some error checking).  Space for array is allocated, so
 *		be sure to pass a pointer to the data pointer. Got it?
 *	returns:
 *			0	->	ok
 *			-1	->	error reading record
 *			-2	->	error allocating space for data
 */
int	xdr_getrecord2(head,array,xdrs)
	ahhed	*head;
	char	**array;
	XDR	*xdrs;
{
	int	ierr = 0;
	int	xdr_gethead();
	char	*mkdatspace();

	if(xdr_gethead(head, xdrs) != 1) {
		ierr = -1;
		return(ierr);
	}

	*array= mkdatspace(head);
	if(*array == NULL)
	{
		ierr= -2;
		return(ierr);
	}

	if(xdr_getdata(head,*array,xdrs) < 0)
		ierr= -1;

	return(ierr);
}


/*	xdr_gogethead
 *		gets n-th header from the xdr stream pointed to by
 *		xdrs and returns this header in the structure
 *		head.
 *	returns:
 *			0	->	OK
 *			-1	->	stream not long enough
 *			-2	->	error reading header
 */
int	xdr_gogethead(n,head,xdrs)
	int	n;
	ahhed	*head;
	XDR	*xdrs;
{
	int	ierr = 0;

	(xdr_tohead(n,xdrs) == n) ? ((xdr_gethead(head,xdrs) < 1) ? (ierr = -2) : (ierr = 0)) : (ierr = -1);
	return(ierr);
}


/*	xdr_gogetrecord
 *		gets n-th record (header and data) from the xdr stream
 *		pointed to by xdrs and places it in head and array.
 *		Calling routine must allocate enough space.
 *	returns:
 *			0	->	OK
 *			-1	->	stream not long enough
 *			-2	->	error reading record
 */
int	xdr_gogetrecord(n,head,array,xdrs)
	int	n;
	ahhed	*head;
	char	*array;
	XDR	*xdrs;

{
	int	ierr = 0;

	(xdr_tohead(n,xdrs) == n) ? ((xdr_getrecord(head,array,xdrs) < 0) ? (ierr = -2) : (ierr = 0)) : (ierr = -1);
	return(ierr);
}






int xdr_ahhead(XDR *xdrsp, ahhed *ahheadp)
{
	u_int l;
	char **pp, *p;
	float **ppf, *pf;

	l = CODESIZE;
	p = ahheadp->station.code;
	pp = &p;
	if (!xdr_bytes(xdrsp, pp, &l, (u_int) CODESIZE))
		return(0);
	l = CHANSIZE;
	p = ahheadp->station.chan;
	pp = &p;
	if (!xdr_bytes(xdrsp, pp, &l, CHANSIZE))
		return(0);
	l = STYPESIZE;
	p = ahheadp->station.stype;
	pp = &p;
	if (!xdr_bytes(xdrsp, pp, &l, STYPESIZE))
		return(0);
	if (!xdr_float(xdrsp, &ahheadp->station.slat))
		return(0);
	if (!xdr_float(xdrsp, &ahheadp->station.slon))
		return(0);
	if (!xdr_float(xdrsp, &ahheadp->station.elev))
		return(0);
	if (!xdr_float(xdrsp, &ahheadp->station.DS))
		return(0);
	if (!xdr_float(xdrsp, &ahheadp->station.A0))
		return(0);
	for (l = 0; l < NOCALPTS; l++) {
		if (!xdr_float(xdrsp, &ahheadp->station.cal[l].pole.r))
			return(0);
		if (!xdr_float(xdrsp, &ahheadp->station.cal[l].pole.i))
			return(0);
		if (!xdr_float(xdrsp, &ahheadp->station.cal[l].zero.r))
			return(0);
		if (!xdr_float(xdrsp, &ahheadp->station.cal[l].zero.i))
			return(0);
	}
	if (!xdr_float(xdrsp, &ahheadp->event.lat))
		return(0);
	if (!xdr_float(xdrsp, &ahheadp->event.lon))
		return(0);
	if (!xdr_float(xdrsp, &ahheadp->event.dep))
		return(0);
	if (!xdr_short(xdrsp, &ahheadp->event.ot.yr))
		return(0);
	if (!xdr_short(xdrsp, &ahheadp->event.ot.mo))
		return(0);
	if (!xdr_short(xdrsp, &ahheadp->event.ot.day))
		return(0);
	if (!xdr_short(xdrsp, &ahheadp->event.ot.hr))
		return(0);
	if (!xdr_short(xdrsp, &ahheadp->event.ot.mn))
		return(0);
	if (!xdr_float(xdrsp, &ahheadp->event.ot.sec))
		return(0);
	l = COMSIZE;
	p = ahheadp->event.ecomment;
	pp = &p;
	if (!xdr_bytes(xdrsp, pp, &l, COMSIZE))
		return(0);
	if (!xdr_short(xdrsp, &ahheadp->record.type))
		return(0);
	if (!xdr_long(xdrsp, &ahheadp->record.ndata))
		return(0);
	if (!xdr_float(xdrsp, &ahheadp->record.delta))
		return(0);
	if (!xdr_float(xdrsp, &ahheadp->record.maxamp))
		return(0);
	if (!xdr_short(xdrsp, &ahheadp->record.abstime.yr))
		return(0);
	if (!xdr_short(xdrsp, &ahheadp->record.abstime.mo))
		return(0);
	if (!xdr_short(xdrsp, &ahheadp->record.abstime.day))
		return(0);
	if (!xdr_short(xdrsp, &ahheadp->record.abstime.hr))
		return(0);
	if (!xdr_short(xdrsp, &ahheadp->record.abstime.mn))
		return(0);
	if (!xdr_float(xdrsp, &ahheadp->record.abstime.sec))
		return(0);
	if (!xdr_float(xdrsp, &ahheadp->record.rmin))
		return(0);
	l = COMSIZE;
	p = ahheadp->record.rcomment;
	pp = &p;
	if (!xdr_bytes(xdrsp, pp, &l, COMSIZE))
		return(0);
	l = LOGSIZE;
	p = ahheadp->record.log;
	pp = &p;
	if (!xdr_bytes(xdrsp, pp, &l, LOGSIZE))
		return(0);
	l = NEXTRAS;
	pf = ahheadp->extra;
	ppf = &pf;
	if (!xdr_array(xdrsp, (caddr_t *)ppf, &l, NEXTRAS, sizeof(float),
		xdr_float))
		return(0);
	

	return(1);
}
#endif

#define LINE_LENGTH 1000

int gblank(char *cha)
{
   int kk, kchar;
   for(kk=0; kk< strlen(cha); kk++)
   {
      /* fprintf(stderr,"<%c>", head.station.code[kk]); */ 
      if(cha[kk]==' ')
      {
	 kchar =kk;
	 break;
      }
   }
   return(kchar);

}
/* #include "../snaps/Src2/Utils/Uwpfif/uwpfif.h" */
/* #include "../snaps/Src2/Utils/Uwdfif/uwdfif.h" */



/** FUNC DEF  **/ void  tojul(int year,int month,int day,int *jul)
{
/*   given a year a month and day, return the julian day
*/
       int yy,mm,dd;
       int c,ya;
       yy = year;
       mm = month;
       dd = day;
       *jul = 0;
       if(mm>2)
          yy = yy+0;
          else
          yy = yy -1;

       if(mm>2)
          mm = mm -3;
          else
          mm = mm+9;

       c = (int)(yy/100);
       ya = yy-100*c;
       *jul= (int)((146097*c)/4)+(int)((1461*ya)/4)
          + (int)((153*mm+2)/5) +dd+1721119;
        return;
}



/*****************/
/* read a sacfile, return pointer to data structure */
/** FUNC DEF **/ void read_sac(char *file ,SacHeader *sh,float  **p)
 
{
  FILE *fp, *fopen();

if(( fp = fopen(file,"r")) == NULL) {
 fprintf(stderr, "Can't Open file %s, aborting\n", file);
 exit(0);
 
}
  

  /* fprintf(stderr, "read_sac: \n"); */
  fread(sh, SACLEN, 1, fp);  /* header */
  free((void *) (*p));   /* free mem before allocating */
  (*p) = (float *)calloc(sh->npts, sizeof(float));
  fread((*p), sizeof(float), sh->npts, fp);
  fclose(fp);
}

/*****************/
/** FUNC DEF **/ SEGYTrace *read_segy(char *fn)
   {
   short *shptr;
   long numsamp;
   
   int  num_read, i;
   FILE *fp, *fopen();

   SEGYTrace *a= (SEGYTrace *)calloc(1, sizeof(SEGYTrace));
      if(( fp = fopen(fn,"r")) == NULL) 
      {
      fprintf(stderr, "Can't Open file %s, aborting\n", fn);
      exit(0);
      }

   fread(&(a->hd), sizeof(char), sizeof(struct SegyHead), fp);

#if 0
printf("data format = %d\n",a->hd.data_form);
printf("  year, day, hour, minute, second %d %d %d %d %d \n",
              a->hd.year, a->hd.day, a->hd.hour, a->hd.minute, a->hd.second);
#endif

     if(a->hd.num_samps==0.0)
	{
	numsamp = a->hd.sampleLength;
	}
     else
	{
        numsamp = a->hd.num_samps;
	}
     
     
   switch (a->hd.data_form) {

   case BIT32:
     a->dat = (int *)calloc((unsigned)numsamp, 4);  

     num_read = fread(a->dat, 4, numsamp, fp);
   break;

   case BIT16:
      a->dat = (int *)calloc((unsigned)numsamp, 4);  
    
     num_read = fread(a->dat, 4, numsamp, fp);

    shptr = (short *) a->dat;
    for(i=numsamp-1;i>=0;i--) a->dat[i] = shptr[i];


   break;

     }



  fclose(fp);

  return a;
}

/*****************/
/** FUNC DEF **/ void  get_one_segy(char *fn, TIME_SERIES *ts)
   {

 
     SEGYTrace *a;
     float dt;
     int i, kk, kchar;
     float scalefac;
     

     a = read_segy(fn);

     if(a->hd.deltaSample>1)
       {
	 dt = (float) a->hd.deltaSample/ 1000000.;
       }
     else
       {
	 dt = (float) a->hd.samp_rate/ 1000000.;
	 
       }


     /* fprintf(stderr, "IN get_one_segy\n"); */


   
/*
      fprintf(stderr, "%s %ld %d %f %d %d %d %d %d %d\n", fn, a->hd.num_samps, a->hd.sampleLength, 
 	     dt, a->hd.year, a->hd.day , a->hd.hour, a->hd.minute,
 	     a->hd.second, a->hd.m_secs); 
*/

     if(a->hd.num_samps==0.0)
	{
	ts->numsamp = a->hd.sampleLength;
	}
     else
	{
        ts->numsamp = a->hd.num_samps;
	}
     
     
     ts->year = a->hd.year;
     ts->jday = a->hd.day;
     ts->hour = a->hd.hour;
     ts->minute = a->hd.minute;
     ts->sec  = (float)a->hd.second + (float)a->hd.m_secs/1000.0;
     


     ts->epoch.year = a->hd.year;
     ts->epoch.julday = a->hd.day;
     ts->epoch.hour = a->hd.hour;
     ts->epoch.minute = a->hd.minute;
     ts->epoch.sec  = (float)a->hd.second + (float)a->hd.m_secs/1000.0;
     


     ts->max = 0.0; 
     ts->min =0.0;


     ts->deltat = dt;
     
     ts->t1 = 0.0;
      ts->t2 = ts->numsamp*dt;
     ts->reference_time  = 0.0;
     ts->origin_time  =   a->hd.hour+a->hd.minute/60.0+ts->sec/(3600.0) ;
     
     ts->amp = (float *)calloc((unsigned)ts->numsamp , sizeof(float));  
     /* ts->ampbak = (float *)calloc((unsigned)ts->numsamp , sizeof(float)); */  

                if (a->hd.gainConst == 0)
                        a->hd.gainConst = 1;
                if (a->hd.scale_fac == 0)
                        a->hd.scale_fac = 1;
 
                scalefac = (float) a->hd.scale_fac / (float) a->hd.gainConst;
 
     ts->scale_factor = scalefac;
     
     ts->max = scalefac *a->dat[0];
     ts->min = scalefac *a->dat[0];
     
     for (i = 0; i < ts->numsamp; i++) 
      {
     /* if( (i%10000)==0) fprintf(stderr,"%d %d\n", i, a->dat[i]); */

     /* fprintf(stderr,"%d %d\n", i, a->dat[i]); */
	/*************   units are now in volts  ****************/

      ts->amp[i] = scalefac *a->dat[i];
      /* ts->ampbak[i] = scalefac *a->dat[i]; */
      
      if( ts->amp[i]>ts->max) ts->max=ts->amp[i];
      if( ts->amp[i]<ts->min) ts->min=ts->amp[i];
      
      }
    	 
     kchar = strlen(a->hd.station_name)+1;
	 
	  
     for(kk=0; kk< strlen(a->hd.station_name); kk++)
     {
	      
	if(a->hd.station_name[kk]==' ')
	{
	   kchar =kk;
	   break;
	}
     }
     /* fprintf(stderr,"kchar = %d\n" , kchar); */ 
     strncpy(ts->id.staname,a->hd.station_name, kchar);
     ts->id.staname[kchar] = '\0';



     kchar = strlen(a->hd.channel_name)+1;
	 
	  
     for(kk=0; kk< strlen(a->hd.channel_name); kk++)
     {
	      
	if(a->hd.channel_name[kk]==' ')
	{
	   kchar =kk;
	   break;
	}
     }
    /*  fprintf(stderr,"kchar = %d %s\n" , kchar, a->hd.channel_name); */
     strncpy(ts->id.comp,a->hd.channel_name, kchar);
     ts->id.comp[kchar] = '\0';

     strcpy(ts->id.filename, fn);
     strcpy(ts->id.id_string, fn);

     free(a->dat);
     free(a);
     
         
     

   }
/*************done with segy********************/

/** FUNC DEF **/ void  get_one_sac(char *fn, TIME_SERIES *ts)
   {

   SacHeader sh;
   int i;
   int kchar;

   float *sacd=NULL; /* sac data pointer */


   read_sac(fn, &sh, &sacd);

    fprintf(stderr, "done reading in read_sac\n"); 
   
  
   ts->deltat = (float) sh.delta;
      ts->t1 = 0.0;
   ts->t2 = sh.e - sh.b;


  ts->numsamp = ((ts->t2 - ts->t1) / ts->deltat) + 1;


   fprintf(stderr, "done reading in read_sac: t1=%f t2=%f e=%f b=%f\n",ts->t1,  ts->t2  , sh.e, sh.b    );  

     fprintf(stderr, "done reading in read_sac: deltat=%f numsamp=%ld npts=%d\n",ts->deltat,  ts->numsamp  , sh.npts    );  

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

 
     ts->amp =    (float *)calloc((unsigned)ts->numsamp , sizeof(float));  
     /* ts->ampbak = (float *)calloc((unsigned)ts->numsamp , sizeof(float));  */ 

     ts->max =  sacd[0];
     ts->min =  sacd[0];

     ts->reference_time  = 0.0;


	/*************   units are now in volts ???  I am not sure ****************/
     fprintf(stderr, "setting up ts data\n"); 
   

     for (i = 0; i < ts->numsamp; i++) 
      {
     if( (i%1000)==0) fprintf(stderr,"%d %g\n", i, sacd[i]); 
      ts->amp[i] = sacd[i];
      /* ts->ampbak[i] = sacd[i]; */
      
      if( ts->amp[i]>ts->max) ts->max=sacd[i];
      if( ts->amp[i]<ts->min) ts->min=sacd[i];
      
      }

     if(ts->max==ts->min)
     {

	ts->min = ts->max-1;

     }


     kchar = gblank(sh.kstnm);
      fprintf(stderr,"kstnm <%s> %d\n", sh.kstnm, kchar); 
     strncpy(ts->id.staname,sh.kstnm , kchar);
     ts->id.staname[kchar] = '\0';  
      fprintf(stderr,"new  <%s>\n", ts->id.staname); 

     kchar = gblank(sh.kcmpnm);
      fprintf(stderr,"kcmpnm <%s> %d\n", sh.kcmpnm, kchar);
     strncpy(ts->id.comp ,sh.kcmpnm , kchar);
     ts->id.comp[kchar] = '\0';
     fprintf(stderr,"new  <%s>\n", ts->id.comp); 



     strcpy(ts->id.filename, fn);
     strcpy(ts->id.id_string, fn);
          fprintf(stderr, "done reading in read_sac: min=%f max=%f\n",ts->min, ts->max    ); 
   free(sacd);
     
     

   }
/***************done with sac******************/
#if USE_AH_XDR
/*************start with ah********************/

/** FUNC DEF **/ void  get_one_ah(char *fn, TIME_SERIES *ts)
   {

     XDR	xdr_in;
     ahhed	head;
     int		 y, m, d, inine, jul, kk, kchar;
     long	nbytes, n;

     /* char	*array; */

     float *array;
     
	
   FILE *fp, *fopen();

   
      if(( fp = fopen(fn,"r")) == NULL) 
      {
      fprintf(stderr, "Can't Open file %s, aborting\n", fn);
      exit(0);
      }


     
     xdrstdio_create(&xdr_in, fp , XDR_DECODE);
     while(xdr_gethead(&head, &xdr_in) > 0)
       {
	 
	 head.station.code[5]= '\0';
	 head.station.chan[5]= '\0';
	 head.station.stype[7]= '\0';
	 head.event.ecomment[79]= '\0';
	 head.record.rcomment[79]= '\0';
	 head.record.log[LOGSIZE-1]= '\0';
	 
	 if(strlen(head.station.code) == 0)
	   sprintf(head.station.code,"null");
	 
	 if(strlen(head.station.chan) == 0)
	   sprintf(head.station.chan,"null");
	 
	 if(strlen(head.station.stype) == 0)
	   sprintf(head.station.stype,"null");
	 
	 if(strlen(head.event.ecomment) == 0)
	   sprintf(head.event.ecomment,"null");
	 
	 if(strlen(head.record.rcomment) == 0)
	   sprintf(head.record.rcomment,"null");
	 
	 if(strlen(head.record.log) == 0)
	   sprintf(head.record.log,"null;");
  
	 /* fprintf(stderr, "done reading in ah data\n"); */
	  /* fprintf(stderr,"code:\t%s\n",head.station.code); */ 
	  /* fprintf(stderr,"code:<%s>",head.station.code); */ 
	  kchar = strlen(head.station.code)+1;
	  /* fprintf(stderr,"in get_ah %d\n", strlen(head.station.code) ); */
	  
	   for(kk=0; kk< strlen(head.station.code); kk++)
	     {
	       /* fprintf(stderr,"<%c>", head.station.code[kk]); */ 
	      if(head.station.code[kk]==' ')
		{
		  kchar =kk;
		  break;
		}
	     }
	  /* fprintf(stderr,"kchar = %d\n" , kchar); */ 
	 strncpy(ts->id.staname, head.station.code, kchar);
	 ts->id.staname[kchar] = '\0';
	  /* fprintf(stderr,"staname<%s>\n",ts->id.staname); */ 


	  kchar = strlen(head.station.chan)+1;
	  /* fprintf(stderr,"in get_ah %d\n", strlen(head.station.code) ); */
	  
	   for(kk=0; kk< strlen(head.station.chan); kk++)
	     {
	       /* fprintf(stderr,"<%c>", head.station.code[kk]); */ 
	      if(head.station.chan[kk]==' ')
		{
		  kchar =kk;
		  break;
		}
	     }
	  /* fprintf(stderr,"kchar = %d\n" , kchar); */ 
	 strncpy(ts->id.comp, head.station.chan, kchar);
	 ts->id.comp[kchar] = '\0';
	  /* fprintf(stderr,"staname<%s>\n",ts->id.staname);  */
	  /* fprintf(stderr,"comp name<%s>\n",ts->id.comp);  */



	 ts->id.latsta = head.station.slat;
	 ts->id.lonsta = head.station.slon;
	 ts->id.depsta = head.station.elev;

	 ts->id.latsrc	 = head.event.lat;
	 ts->id.lonsrc	 = head.event.lon;
	 ts->id.depsrc	 = head.event.dep;



	 
	 ts->deltat = (float) head.record.delta;
	 ts->t1 = 0.0;
	 
	 
	 ts->numsamp = head.record.ndata;
	 ts->t2 =    ts->deltat*ts->numsamp;

	 ts->year = head.record.abstime.yr;
	 y = head.record.abstime.yr;
	 m = 1;
	 d = 1;
	 
	 tojul(y,m,d, &inine);
	 m = head.record.abstime.mo;
	 d = head.record.abstime.day;
	 tojul(y,m,d, &jul);
	 jul = jul - inine + 1;
	 
	 
	 
	 ts->jday= jul;
	 ts->month = head.record.abstime.mo;
	 
	 
	 ts->day=head.record.abstime.day;
	 ts->hour=head.record.abstime.hr;
	 ts->minute= head.record.abstime.mn;
	 ts->sec= head.record.abstime.sec;
	 
	 
	 ts->epoch.year = ts->year;
	 
  
	 ts->epoch.julday = ts->jday;
	 ts->epoch.hour =  ts->hour;
	 ts->epoch.minute = ts->minute;
	 ts->epoch.sec  = ts->sec;
	 
	 ts->reference_time  = 0.0;
	 ts->scale_factor = head.station.A0;
	 
	 nbytes = (head.record.ndata)*size(&head);

	if ((array = (float *) calloc((unsigned) (head.record.ndata), size(&head))) == NULL) {
	    fprintf(stderr, "Error allocating space in get_seis\n");
	    exit(-2);
	}
	 
	 xdr_getdata(&head, array, &xdr_in);	
	 
	 n= head.record.ndata;
	 ts->amp = (float *)calloc((unsigned)ts->numsamp , sizeof(float));  
	 /* ts->ampbak = (float *)calloc((unsigned)ts->numsamp , sizeof(float));   */
		
	 ts->max =  array[0];
	 ts->min =  array[0];

	 /* fprintf(stderr,"ah stuff\n nsamp=%d deltat=%f type=%d\n", */
/* 		 ts->numsamp, ts->deltat,  head.record.type); */
	 
	 
	 switch(head.record.type)
	   {
	   case FLOAT:			/* floats	*/
	     /* fptr= (float *)array; */
	     for(n=0; n<head.record.ndata; n++)
	       {
				/* fprintf( file_out, "%e\n", *fptr ); */
		 /* 	++fptr; */
		 ts->amp[n] = array[n];
		/*   ts->ampbak[n] = array[n]; */
		 if( ts->amp[n]>ts->max) ts->max=array[n];
		 if( ts->amp[n]<ts->min) ts->min=array[n];
	       }
	     break;
	     
	     
	     
	   case DOUBLE:			/* doubles	*/
	     /* dptr= (double *)array; */
	     for(n=0; n<head.record.ndata; n++)
	       {
		 ts->amp[n] = array[n];
		 /* ts->ampbak[n] = array[n]; */
		 
		 if( ts->amp[n]>ts->max) ts->max=array[n];
		 if( ts->amp[n]<ts->min) ts->min=array[n];
				/* fprintf( file_out, "%e\n", *dptr ); */
				/* ++dptr; */
			}
	     break;
	     
	   }				/*	end switch	*/
	 
	 free((char *)array);
	 
	 
	 
       }
     
    
     strcpy(ts->id.filename, fn);
     strcpy(ts->id.id_string, fn);


     /* fprintf(stderr,"ah file %s\n", fn); */
     /*  fprintf(stderr,"ah file %s\n", ts->id.filename ); */
    /* ts->amp = (float *)calloc((unsigned)ts->numsamp , sizeof(float));  
     ts->ampbak = (float *)calloc((unsigned)ts->numsamp , sizeof(float));  */


     
     fclose(fp);

    
	 
     
   }
#endif


/** FUNC DEF */ void CALL_JGETSEIS(char **fn, int *kind, double *x, int *n, double *dt, int *DATE1, double *sec)
{
 
   int i;

   TIME_SERIES ts;
   /* char *te; */

   /* strcpy(te,fn[0]); */

   /* fprintf(stderr, "%s\n", fn[0]); */

   
   switch(*kind)
   {
      case 1:
	 get_one_segy(fn[0],  &ts);
	 break;
      case 2:
	 get_one_sac(fn[0],  &ts);
	 break;
      case 3:
#if USE_AH_XDR
	 get_one_ah(fn[0],  &ts);
#endif

	 break;
      default:
	 get_one_segy(fn[0],  &ts);
	 break;
   }
   
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

   *sec = ts.sec;


   free(ts.amp);
   /* free(ts.ampbak); */

 
}
/** FUNC DEF */ void CALL_JSETSEIS(char **fn, int *kind, int *n, double *dt, int *DATE1, double *sec, char **stn, char **cmpn)
{
 
   /* int i; */

   TIME_SERIES ts;
   /* char *te; */

   /* strcpy(te,fn[0]); */

   /* fprintf(stderr, "%s\n", fn[0]); */

   
   switch(*kind)
   {
      case 1:
	 get_one_segy(fn[0],  &ts);
	 break;
      case 2:
	 get_one_sac(fn[0],  &ts);
	 break;
      case 3:
#if USE_AH_XDR
	 get_one_ah(fn[0],  &ts);
#endif
	 break;
      default:
	 get_one_segy(fn[0],  &ts);
	 break;
   }
   
   *n =  ts.numsamp;
   *dt = ts.deltat;

   DATE1[0] = ts.year;
   DATE1[1] = ts.jday;
   DATE1[2] = ts.hour;
   DATE1[3] = ts.minute;

   *sec = ts.sec;

   /* fprintf(stderr, "station: %s\ncomp: %s\n", ts.id.staname, ts.id.comp); */

   strcpy(stn[0], ts.id.staname);
   strcpy(cmpn[0], ts.id.comp);

   free(ts.amp);
   /* free(ts.ampbak); */

 
}





