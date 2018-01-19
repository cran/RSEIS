\name{trapz}
\alias{trapz}

\title{Integrate using trapezoidal rule}
\description{
  Integrate using trapezoidal rule
}
\usage{
trapz(y, dt, rm.mean=TRUE)
}

\arguments{
  \item{y}{Input signal}
  \item{dt}{sample interval time, seconds}
  \item{rm.mean}{logical, whether to remove the mean prior to integration (TRUE) }
 
}

\value{
  vector: Integrated signal
}

\author{Jonathan M. Lees<jonathan.lees@unc.edu>}

\examples{
x <- rnorm(100)
trapz(x, 0.01)

}

\keyword{misc}
