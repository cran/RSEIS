\name{Jtim}
\alias{Jtim}
\alias{JtimL}

\title{Decimal Julian Day}
\description{
convert JD, HR, MIN SEC to Decimal Julian Day
}
\usage{
Jtim(jj, hr = hr, mi = mi, sec = sec, yr=NULL)
JtimL(j)
}

\arguments{
  
  \item{jj}{Julian day}
  \item{hr}{Hour}
  \item{mi}{Minute}
  \item{sec}{Second}
  \item{yr}{year, default = NULL}

  or
    \item{j}{list of the above}

  
}

\value{
 Julian day
}
\details{
  Using a NULL value for yr gives the fractional julian day
  in a year.  If yr is a legitimate year, then the EPOCH number of days
  from 1972 are added onto the fractional julian day.

  }
\author{Jonathan M. Lees<jonathan.lees@unc.edu>}

\seealso{secdif}
\examples{
Jtim( 9 , hr= 14 , mi= 53 ,sec= 16.7807606880087 )

########   or,
j = list(jd=9 , hr= 14 , mi= 53 ,sec= 16.7807606880087)

JtimL(j)




}

\keyword{misc}
