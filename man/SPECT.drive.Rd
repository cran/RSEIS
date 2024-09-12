\name{SPECT.drive}
\alias{SPECT.drive}

\title{Interactive Spectrogram Driver}
\description{
  Interactive Spectrogram Driver
}
\usage{
SPECT.drive(Xamp, DT = 0.008, NEW = TRUE, STAMP = NULL ,
 freqlim=c(0, 20, 0, 20), winparams=c(4096,256, 204 ))
}

\arguments{
  \item{Xamp}{signal trace}
  \item{DT}{deltaT sample interval, s}
  \item{NEW}{logical, TRUE=recalculate spectrum}
  \item{STAMP}{character stamp for identification}
  \item{freqlim}{vector of 4 frequency limits: min max for calculations,
  min max for display.  Default=see below }
  \item{winparams}{vector of 3 window parameters: Number of points for
    FFT, number of time samples for window, number of overlap samples:
    default=see below }
  
}
\details{
  Interactive buttons are set internally.  The parameters freqlim and
  winparams can be changed - these are simply the starting parameters
  for the initial display.

  For winparams, the parameters are set to be appropriate
  for sample rates of typical seismic data, 100-125 samples per second.
  The number of points in the FFT are initially set to 4096 and the time
  window is set to 256.  The overlap is calculated by subtracting 20
  percent of the time
  window, so the overlap is 80 percent.
  Of course, the number of samples in a window must be less than the
  length of input time series.






  
}


\value{
Graphical Side Effects
}

\author{Jonathan M. Lees<jonathan.lees.edu>}

\seealso{plotevol, RPMG}
\examples{
data(CE1)

#########  Xamp = CE1$y[CE1$x>5.443754 & CE1$x<5.615951]

Xamp = CE1$y
plot(Xamp, type='l')


DT = CE1$dt

if(interactive() ) {
SPECT.drive(Xamp, DT = DT, NEW = TRUE, STAMP = NULL) }

}

\keyword{iplot}
