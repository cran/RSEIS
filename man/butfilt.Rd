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
      \item{LP}{ for low pass  }
     \item{ HP}{  for high pass }
    \item{ BP}{  for band pass }
      \item{ BR}{  for band reject }
   \item{ BU}{  for Butterworth }
   \item{BE}{  for Bessel }
    \item{ C1}{  for Chebyshev type 1 }
               \item{ C2}{ for Chebyshev type 2 }

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
