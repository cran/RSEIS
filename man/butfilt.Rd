\name{butfilt}
\alias{butfilt}

\title{Butterworth filter}
\description{
 design and apply butterworth low/high/band pass filters.
}
\usage{
butfilt(a, fl=0, fh=0.5, deltat=1, type="BP", proto="BU",
 npoles=5, chebstop=30.0, trbndw=0.3)
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
}
\details{
  creation of butfilt is a described by the following arguments:
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

}
\value{
filtered time series 
}

\author{Jonathan M. Lees<jonathan.lees.edu>}


\examples{
data(CE1)

ts1  <-  CE1$y
zz  <-  butfilt(ts1, 1, 15,  CE1$dt, "LP" ,  "BU")


}

\keyword{misc}
