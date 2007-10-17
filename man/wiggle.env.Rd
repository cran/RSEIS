\name{wiggle.env}
\alias{wiggle.env}

\title{Plot time series envelope}
\description{
 Gets an envelope and lpots on a time series
}
\usage{
wiggle.env(x, y)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{x-coordinate}
  \item{y}{y-coordinate}
}
\details{
 Uses Peaks and smooth.pline to estimate envelope
}
\value{
  \item{list}{structure from smooth.spline} 
}

\author{Jonathan M. Lees<jonathan.lees.edu>}

\seealso{smooth.spline, peaks, hilbert}
\examples{

##  data("CE1.Example.RDATA")
##   load("CE1.Example.RDATA")
data(CE1)
plot(CE1$x, CE1$y, type='l')
wiggle.env(CE1$x, CE1$y)
we = wiggle.env(CE1$x, CE1$y)
lines(we$x, we$y, col='red')

}

\keyword{misc}
