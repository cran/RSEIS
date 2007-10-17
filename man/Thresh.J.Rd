\name{Thresh.J}
\alias{Thresh.J}
\title{Threshold Adjuster}
\description{
  determine cut off for ratio curve
 }
\usage{
Thresh.J(y, thresh)
}

\arguments{
  \item{y}{signal}
  \item{thresh}{inital threshold}
}
\details{
  Attempts to automatically optimize the threshold for automated
  picking.
  Used deep in picking algorithm.
}
\value{
 list(J=J, L=L)
}

\author{Jonathan M. Lees<jonathan.lees.edu>}


\keyword{misc}