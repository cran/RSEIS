\name{getpfile}
\alias{getpfile}

\title{Get Pick File}
\description{
 Read Pick File to R
}
\usage{
getpfile(uwpickfile, stafile = NULL)
}

\arguments{
  \item{uwpickfile}{pick file}
  \item{stafile}{station file}
}
\details{
  University of washington Format pickfiles are used.  See EmptyPickfile
  for the structure stored.
}
\value{
pickfile structure
}

\author{Jonathan M. Lees<jonathan.lees.edu>}

\seealso{EmptyPickfile}

\keyword{misc}
