\name{hilbert}
\alias{hilbert}

\title{Hilbert Transform}
\description{
 Hilbert transform
}
\usage{
hilbert(x)
}

\arguments{
  \item{x}{time series vector}
}
\details{
  Returns the hilbert transform.  Used for calculating
  the envelope function.
}
\value{
 vector
}

\author{Jonathan M. Lees<jonathan.lees.edu>}

\seealso{fft, envelope }
\examples{
x = rnorm(100)
y=hilbert(x)

}

\keyword{misc}
