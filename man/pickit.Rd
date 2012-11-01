\name{pickit}
\alias{pickit}

\title{ Automatic Picking Algorithm}
\description{
  Automatic Picking Algorithm
}
\usage{
pickit(ay, deltat = 0.008, MED = 225, FRWD = 8, BKWD = 8,
 sbef = 1, saft = 6, thresh = 2, Tthresh2 = 7,
stretch = 1000, flo = 0.1, fhi = 5, Kmin = 7,
dthresh = 0.01, threshbot = 1.01)
}

\arguments{
  \item{ay}{signal}
  \item{deltat}{ sample rate }
  \item{MED}{use median smoothing? }
   \item{FRWD}{forward window, s}
  \item{BKWD}{backward window }
  \item{sbef}{seconds before}
  \item{saft}{seconds after}
  \item{thresh}{threshold 1}
  \item{Tthresh2}{threshold 2}
  \item{stretch}{stretch factor}
  \item{flo}{low frequency for BP filter}
  \item{fhi}{low frequency for BP  filter}
 \item{Kmin}{min number of picks per window}
  \item{dthresh}{delta threshold }
  \item{threshbot}{threshold bottom limit }
}
\details{
  used internally.
  This code uses several methods for getting best pick.
}
\value{
  list(RAT=A$rat, x=x, ay=ay, fy=fy, deltat=deltat, J=J$J ,
  Z=Z, a1=a1, a2=a2, thresh=thresh, Tthresh2=Tthresh2, Kmin=Kmin)
}

\author{Jonathan M. Lees<jonathan.lees.edu>}

\seealso{ETECTG}

\keyword{misc}