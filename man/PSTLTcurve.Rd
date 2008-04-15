\name{PSTLTcurve}
\alias{PSTLTcurve}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Short Term/Long Term curve}
\description{
 ST/LT ratio curve for sutomated picking routines
}
\usage{
PSTLTcurve(y, dt = 0.008, fwlen = 125, bwlen = 125, perc = 0.05, stretch = 1000, MED = 255, PLOT = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{y}{signal}
  \item{dt}{deltaT (s)}
  \item{fwlen}{forward window }
  \item{bwlen}{backward window}
  \item{perc}{percent cut-off }
  \item{stretch}{stretch curve }
  \item{MED}{Median smoothing parameter }
  \item{PLOT}{logical, TRUE=PLOT}
}

\value{

  list(flag=1, ind=ix, eye=eye, mix=mix, SNR=SNR,  s2=s2, rat=therat)
  
  
  \item{flag}{flag on success}
  \item{ind}{index of pick estimate 1}
   \item{eye}{index of pick estimate 2}
   \item{mix}{index of pick estimate 3}
   \item{SNR}{Signal/Noise ratio}
   \item{s2}{sum squared}
   \item{rat}{ratio curve}

}
\references{ ~put references to the literature/web site here ~ }
\author{Jonathan M. Lees<jonathan.lees.edu>}
\note{ 
}
\seealso{}
\examples{

require(stats)

data(CE1)
plot(CE1$x, CE1$y, type='l')

z = CE1$y[ CE1$x>5.352622 & CE1$x<5.589836]

PSTLTcurve(z, dt = CE1$dt, fwlen = 125, bwlen = 125, perc = 0.05,
stretch = 1000, MED = 255, PLOT = FALSE)

}

\keyword{misc}
\keyword{hplot}
