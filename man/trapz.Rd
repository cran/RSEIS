\name{trapz}
\alias{trapz}

\title{Integrate using trapezoidal rule}
\description{
  Integrate using trapezoidal rule
}
\usage{
trapz(y, dt)
}

\arguments{
  \item{y}{Input signal}
  \item{dt}{sample interval time, seconds}
}

\value{
  vector: Integrated signal
}

\author{Jonathan M. Lees<jonathan.lees@unc.edu>}

\examples{
x = rnorm(100)
trapz(x, 0.01)

}

\keyword{misc}
