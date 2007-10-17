\name{GETARAIC}
\alias{GETARAIC}

\title{Auto-Regressive AIC estimate of arrival time}
\description{
 Auto-Regressive AIC for arrival estimate, signal detection
}
\usage{
GETARAIC(z4, deltat = 0.008, Mar = 8, O1 = 2, O2 = 0.2, WW = 2, T1 = 1, PLOT = FALSE)
}

\arguments{
  \item{z4}{signal time series}
  \item{deltat}{sample rate,s}
  \item{Mar}{AR Model Order}
  \item{O1}{window before}
  \item{O2}{window after}
  \item{WW}{window length}
  \item{T1}{initial time guess}
  \item{PLOT}{logical, TRUE =plot}
}
\details{
 Method of Sleeman for automatic phase determination.
}
\value{
  
  \item{Taic}{Arrival time of wave}
 
}
\references{Sleeman}
\author{Jonathan M. Lees<jonathan.lees.edu>}

\seealso{PSTLTcurve}
\examples{


data(CE1)
plot(CE1$x, CE1$y, type='l')

Xamp = CE1$y[CE1$x>4.443754 & CE1$x<6.615951]


araict = GETARAIC(Xamp, deltat=CE1$dt, T1=1, Mar=8, O1=2, O2=0.2, WW=2,   PLOT=TRUE)

}

\keyword{misc}
\keyword{hplot}
