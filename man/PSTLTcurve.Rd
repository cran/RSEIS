\name{PSTLTcurve}
\alias{PSTLTcurve}

\title{Short Term/Long Term curve}
\description{
 ST/LT ratio curve for sutomated picking routines
}
\usage{
PSTLTcurve(y, dt = 0.008, fwlen = 125, bwlen = 125,
 perc = 0.05, stretch = 1000, MED = 255, PLOT = FALSE)
}

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

\author{Jonathan M. Lees<jonathan.lees.edu>}

\examples{

require(stats)

data(CE1)
plot(CE1$x, CE1$y, type='l')

z <- CE1$y[ CE1$x>5.352622 & CE1$x<5.589836]
x <- CE1$x[ CE1$x>5.352622 & CE1$x<5.589836]

G <- PSTLTcurve(z, dt = CE1$dt, fwlen = 10,
bwlen = 10, perc = 0.05,
stretch = 10, MED = 11, PLOT = FALSE)

### get time from beginning of trace
tpick <- x[G$ind]
abline(v=x[G$ind], col='red', lty=2)


}

\keyword{misc}
\keyword{hplot}
