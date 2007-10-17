\name{cleanpickfile}
\alias{cleanpickfile}

\title{Clean up Pickfile structure}
\description{
Given a pickfile, clean out stations that do not ocnform
}
\usage{
cleanpickfile(P)
}

\arguments{
  \item{P}{Pickfile list}
}
\details{
 stations with name="" are removed
}
\value{
  \item{P}{Pickfile list}
}

\author{Jonathan M. Lees<jonathan.lees.edu>}

\seealso{EmptyPickfile}
\examples{
P = EmptyPickfile()
cleanpickfile(P)
}

\keyword{misc}
