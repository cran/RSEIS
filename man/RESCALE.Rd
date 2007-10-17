\name{RESCALE}
\alias{RESCALE}

\title{Rescale a vector to fit in a certain range }
\description{
 Rescale a vector to fit in a certain range
}
\usage{
RESCALE(x, nx1, nx2, minx, maxx)
}

\arguments{
  \item{x}{vector}
  \item{nx1}{new minimum}
  \item{nx2}{new maximum}
  \item{minx}{old min}
  \item{maxx}{old max}
}
\details{
 Used for graphics.
}
\value{
 scale vector is returned
}

\author{Jonathan M. Lees<jonathan.lees.edu>}


\examples{
x = rnorm(10)
RESCALE(x, 3, 9, min(x), max(x) )
}

\keyword{misc}
