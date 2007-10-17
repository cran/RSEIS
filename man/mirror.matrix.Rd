\name{mirror.matrix}
\alias{mirror.matrix}

\title{mirror matrix}
\description{
  mirrored representation of image matrix
}
\usage{
mirror.matrix(x)
}

\arguments{
  \item{x}{matrix }
}
\details{
  Used for flipping the output of the wavelet transform for
  more convenient plotting.
}
\value{
  matrix
}
\references{Rwave, plotwlet, wlet.do, wlet.drive}
\author{Jonathan M. Lees<jonathan.lees.edu>}

\examples{

xy = matrix(rnorm(100), ncol=10)
mirror.matrix(xy)


}

\keyword{misc}
