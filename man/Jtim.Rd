\name{Jtim}
\alias{Jtim}
\alias{JtimL}

\title{Decimal Julian Day}
\description{
convert JD, HR, MIN SEC to Decimal Julian Day
}
\usage{
Jtim(jj, hr = hr, mi = mi, sec = sec, yr=NULL, origyr=NULL)
JtimL(j)
}

\arguments{
  
  \item{jj}{Julian day}
  \item{hr}{Hour}
  \item{mi}{Minute}
  \item{sec}{Second}
  \item{yr}{year, default = NULL}
  \item{origyr}{default = NULL}	
  or
    \item{j}{list of the above}

  
}

\value{
 Julian day
}
\details{
  Using a NULL value for yr gives the fractional julian day
  in a year.  If yr is a legitimate year, and the origyr is provided,
  then the EPOCH number of days
  from origyr are added onto the fractional julian day.
  The default for origyr is 1972 for most of seismology.

If the dates span a new year, sometimes it is useful to
use the earliest year as the origyr.	



  }
\author{Jonathan M. Lees<jonathan.lees@unc.edu>}

\seealso{secdif}
\examples{
Jtim( 9 , hr= 14 , mi= 53 ,sec= 16.7807606880087 )

Jtim( 9 , hr= 14 , mi= 53 ,sec= 16.7807606880087, yr=2019, origyr=1972 )


########   or,
j = list(jd=9 , hr= 14 , mi= 53 ,sec= 16.7807606880087)

JtimL(j)




}

\keyword{misc}
