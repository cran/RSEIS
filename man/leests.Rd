\name{leests}
\alias{leests}

\title{Time Series Structure}
\description{
  return time series structure
}
\usage{
leests(a, dt = 0.008)
}

\arguments{
  \item{a}{vector signal}
  \item{dt}{sample rate}
}

\value{
  list(y=y, dt=dt)
}

\examples{

x = rnorm(10)

leests(x, dt = 0.01)


}

\author{Jonathan M. Lees<jonathan.lees.edu>}

\keyword{misc}