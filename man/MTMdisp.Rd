\name{MTMdisp}
\alias{MTMdisp}

\title{ MTMdisp}
\description{
Display MTM displacement spectrum.
}
\usage{
MTMdisp(a, f1 = f1, f2 = f2, len2 = 1024, PLOT = FALSE)
}

\arguments{
  \item{a}{seismic velocity trace, as a ts structure (list(y=trace,
    dt=sample rate) }
  \item{f1}{ low frequency }
  \item{f2}{high frequency }
  \item{len2}{length of fft }
  \item{PLOT}{ logical, TRUE=plot }
}
\details{
 Uses Multi-taper estimate of spectrum and divides the spectrum by
 1/(2*pi*f) to get integration of velocity seismogram.
}
\value{
  Returns displacement spectrum.
  Graphical Side effect.
}
\references{Lees, J. M. and Park, J., 1995: Multiple-taper spectral analysis: A stand-alone C-subroutine, \emph{Computers \& Geology}, 21(2), 199-236.}
\author{Jonathan M. Lees<jonathan.lees.edu>}

\seealso{mtapspec  }
\examples{
data(CE1)
xvel = list(y=CE1$y[CE1$x>5.443754 & CE1$x<5.615951], dt=CE1$dt)

len2 = next2(length(xvel$y))
Spec =MTMdisp(xvel, f1=.01, f2=25, len2=len2, PLOT=FALSE )

}

\keyword{hplot}
