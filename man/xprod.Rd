\name{xprod}
\alias{xprod}

\title{Vector Cross Product}
\description{
  Cross product of two vectors
}
\usage{
xprod(A1, A2)
}

\arguments{
  \item{A1}{3 component vector of x,y,z}
  \item{A2}{3 component vector of x,y,z }
}

\value{
 3 component vector of A1 cross  A2
}

\author{Jonathan M. Lees <jonathan.lees@unc.edu>}
\examples{
B1 <- c(4,9,2)
B2 <- c(2,-5,4)

xprod(B1, B2)


}

\keyword{misc}
