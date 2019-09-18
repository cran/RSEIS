\name{butfilt}
\alias{butfilt}

\title{Butterworth filter}
\description{
 Design and apply butterworth low/high/band pass filters.
}
\usage{
butfilt(a, fl=0, fh=0.5, deltat=1, type="BP", proto="BU",
 npoles=5, chebstop=30.0, trbndw=0.3, RM=FALSE, zp=TRUE )
}

\arguments{
  \item{a}{vector signal}
  \item{fl}{low frequency cut-off, default=0 }
  \item{fh}{high frequency cut-off, DEFAULT= (1/2dt) }
  \item{deltat}{sample rate, s, deFAULT=1}
  \item{type}{type of filter, one of c("LP",  "HP","BP" ,"BR" ), DEFAULT="BP" }
  \item{proto}{prototype, c("BU", "BE" , "C1" ,"C2"), DEFAULT="BU"  }
  \item{npoles}{number of poles or order, DEFAULT=5}
  \item{chebstop}{Chebyshev stop band attenuation, DEFAULT=30.0}
  \item{trbndw}{Chebyshev transition bandwidth, DEFAULT=0.3}
  \item{RM}{Remove mean value from trace, default=FALSE}
  \item{zp}{zero phase filter, default=TRUE}
}
\details{
  Creation of butfilt is a described by the following arguments:
  \describe{
    \item{LP}{low pass  }
    \item{HP}{high pass }
    \item{BP}{band pass }
    \item{BR}{band reject }
    \item{BU}{Butterworth }
    \item{BE}{Bessel }
    \item{C1}{Chebyshev type 1 }
    \item{C2}{Chebyshev type 2 }
  }

  Arguments chebstop , trbndw  are ignored for non-chebyshev filters.
  LP and HP filters are seet by specifying fl for HP filters and fh for
  LP filters, the other argumentin each case is ignored.

  Mean values should be removed prior to calling this function, and then set RM=FALSE.
  This is true especially if tapering is applied prior to filtering.

  Zero phase filter is achived by running filter back and forth.  Otherwise
  a single pass is returned.  This should be equivalent to package signal filtfilt (from MATLAB).



}
\value{
Filtered time series. 
}

\author{Jonathan M. Lees<jonathan.lees.edu>}

\seealso{AUGMENTbutfilt}

\examples{
data(CE1)

ts1  <-  CE1$y
zz  <-  butfilt(ts1, fl=1, fh=15,  deltat=CE1$dt, type="LP" ,  proto="BU",
npoles=5 )



###  try plotting:
\dontrun{
### the above, by default, is zero phase.
#####  next filter with non-zero-phase
z2  <-  butfilt(ts1, fl=1, fh=15,  deltat=CE1$dt, type="LP" ,  proto="BU",
npoles=5, zp=FALSE )
ex = seq(from=0, by=CE1$dt, length=length(ts1))

plot(ex,  ts1, type='l')
lines(ex, zz, col='red')
lines(ex, z2, col='blue')


plot(ex[ex<0.5],  ts1[ex<0.5], type='l')
lines(ex[ex<0.5], zz[ex<0.5], col='red')
lines(ex[ex<0.5], z2[ex<0.5], col='blue')



}


}

\keyword{misc}
