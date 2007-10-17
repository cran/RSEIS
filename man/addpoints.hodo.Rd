\name{addpoints.hodo}
\alias{addpoints.hodo}

\title{Add points to a hodogram plot }
\description{
  Add points to a hodogram plot
}
\usage{
addpoints.hodo(nbaz, dt, sx, flag = 1:10, pch = 3, col = 1)
}

\arguments{
  \item{nbaz}{matrix 3 by n }
  \item{dt}{sample interval, s }
  \item{sx}{x vector}
  \item{flag}{output of idpoints.hodo }
  \item{pch}{plot character }
  \item{col}{color for plotting }
}

\value{
  Graphical Side Effect
}

\author{Jonathan M. Lees<jonathan.lees.edu>}

\seealso{PMOT.drive, idpoints.hodo}

\keyword{misc}
\keyword{aplot}
