\name{xtract.trace}
\alias{xtract.trace}
\title{Extract trace}
\description{
  Extract one time series trace from an RSEIS data list
}
\usage{
xtract.trace(GH, sel = 1, WIN = c(0, 1))
}
\arguments{
  \item{GH}{RSEIS list}
  \item{sel}{select trace index}
  \item{WIN}{time window on trace, relative to start}
}

\value{
  \item{vector}{amplitudes}
 
}

\author{Jonathan M. Lees<jonathan.lees@unc.edu>}
\details{
  An attribute of dt (sample time interval)
  is attached to the time series for
  use in plotting.
}

\examples{

data(GH)

x1  <- xtract.trace(GH, sel = 1, WIN = c(0, 1))
plot(x1, type='l')

}
\keyword{misc}
