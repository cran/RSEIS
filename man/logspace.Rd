\name{logspace}
\alias{logspace}

\title{Logarithm}
\description{
  Logarithmically spaced vector
}
\usage{
logspace(d1, d2, n = n)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{d1}{lower frequency}
  \item{d2}{upper frequency}
  \item{n}{number of frequencies}
}
\details{
  generates a row vector of n logarithmically equally spaced
  points between decades \eqn{10^{X1}} and\eqn{10^{X2}}
}
\value{
 vector
}

\author{Jonathan M. Lees<jonathan.lees.edu>}

\examples{

f=logspace(1, 25)

}

\keyword{misc}
