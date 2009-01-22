\name{get.slepians}
\alias{get.slepians}

\title{Get Slepian Tapers}
\description{
  Return a matrix of Slepian tapers 
}
\usage{
get.slepians(npoints = 900, nwin = 5, npi = 3)
}
\arguments{
  \item{npoints}{Number of points to return}
  \item{nwin}{ Number of windows (default =5) }
  \item{npi}{ Pi-Prolate numerber (3)}
}
\details{
  This function onlyu returns the tapers for inspection.  To apply the
  tapers use the function mtapspec.
}
\value{
 Matrix: nwin vectors of npoints  Slepian tapers
}
\references{Lees, J. M. and Park, J., 1995: Multiple-taper spectral
  analysis: A stand-alone C-subroutine, \emph{Computers \& Geology}, 21(2), 199-236.}
\author{Jonathan M. Lees<jonathan.lees.edu>}

\seealso{mtapspec}
\examples{

nwin = 5
npi = 3
npoints = 900
sleps = get.slepians(npoints, nwin, npi)

matplot(sleps, type='l', xlab="Index", ylab="Taper Amplitude")
legend('topleft', legend=1:nwin, lty=1:nwin, col=1:nwin)



}

\keyword{misc}
