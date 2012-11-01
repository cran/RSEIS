\name{makefreq}
\alias{makefreq}

\title{Make Frequency}
\description{
 Create a frequency value for integration and differentiation
}
\usage{
makefreq(n, dt)
}

\arguments{
  \item{n}{number of freqs}
  \item{dt}{deltat}
}

\value{
  vector of frequencies
}

\author{Jonathan M. Lees<jonathan.lees.edu>}

\seealso{INVRft}
\examples{
N <- 256
dt <- 0.008
f <- makefreq(N,dt)


}

\keyword{misc}
