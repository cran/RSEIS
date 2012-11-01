\name{STLTcurve}
\alias{STLTcurve}

\title{Short-term/Long-term Average curve}
\description{
  Get short-term average long-term verage ratio curve for picking
}
\usage{
STLTcurve(y, dt = 0.008, fwlen = 125, bwlen = 125,
stretch = 1000, MED = 255, PLOT = FALSE)
}

\arguments{
  \item{y}{signal}
  \item{dt}{sample rate }
  \item{fwlen}{forward window, number of samples }
  \item{bwlen}{back window length, number of samples }
  \item{stretch}{stretch multiplier}
  \item{MED}{median smoother}
  \item{PLOT}{logical, TRUE=plot diagnostics}
}
\details{
Uses C-code and fast tanking algorithm written at UW
}
\value{
  sample to significant change in ratio curve
}

\author{Jonathan M. Lees<jonathan.lees.edu>}

\seealso{PSTLTcurve }
\examples{

data(CE1)

y  = CE1$y

DT = CE1$dt



sy = STLTcurve(y, dt=DT, fwlen =  25,  bwlen  = 25,
stretch=1000, MED=255, PLOT=FALSE)

par(mfrow=c(2,1))

plot(CE1$x, CE1$y, type='l')
plot(CE1$x,sy$rat, type='l')




}



\keyword{misc}