
\name{GETARAIC}
\alias{GETARAIC}

\title{Auto-Regressive AIC estimate of arrival time}
\description{
 Auto-Regressive AIC for arrival estimate, signal detection
}
\usage{
GETARAIC(z4, DT = 0.008, Mar = 8, O1 = 2, O2 = 0.2, WW = 2, T1 = 1, PLOT = FALSE)
}

\arguments{
  \item{z4}{signal time series}
  \item{DT}{sample rate,s}
  \item{Mar}{AR Model Order}
  \item{O1}{window before, s}
  \item{O2}{window after, s}
  \item{WW}{window length, s}
  \item{T1}{initial guess, number of samples from beginning of trace}
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
Mar=8
z4 = Xamp
DT = CE1$dt
 T1 = 50

O1 = 10*DT
O2 =  10*DT
WW = 10*DT
Nz4 = length(z4)


 araict = GETARAIC(Xamp, DT=CE1$dt, Mar=8, T1=T1, O1=O1, O2=O2, WW=WW,   PLOT=TRUE)


}

\keyword{misc}
\keyword{hplot}
