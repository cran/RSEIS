\name{butfilt}
\alias{butfilt}

\title{Butterworth filter}
\description{
 design and apply butterworth low/high/band pass filters.
}
\usage{
butfilt(a, fl, fh, deltat, type, proto)
}

\arguments{
  \item{a}{vector signal}
  \item{fl}{low frequency cut-off }
  \item{fh}{high frequency cut-off}
  \item{deltat}{sample rate, s}
  \item{type}{type of filter, one of c("LP",  "HP","BP" ,"BR" )  }
  \item{proto}{prototype, c("BU", "BE" , "C1" ,"C2")  }
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

}
\value{
filtered time series 
}

\author{Jonathan M. Lees<jonathan.lees.edu>}


\examples{
data(CE1)

ts1 = CE1$y
zz = butfilt(ts1, 1, 15,  CE1$dt, "LP" ,  "BU")


}

\keyword{misc}
