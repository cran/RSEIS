\name{brune.func}
\alias{brune.func}

\title{Brune Earquake Model}
\description{
 Calculate Forward Brune model
}
\usage{
brune.func(freq, omega0, tstar0, fc, alpha, gamma)
}

\arguments{
  \item{freq}{frequency vector}
  \item{omega0}{low freq asymptote }
  \item{tstar0}{T-star value}
  \item{fc}{corner frequency }
  \item{alpha}{alpha parameter}
  \item{gamma}{gamma parameter}
}
\details{
  Brune model.
}
\value{
  returns displacement spectrum from given parameters
}
\references{}
\author{Jonathan M. Lees<jonathan.lees.edu>}

\seealso{brune.doom}

\keyword{misc}
