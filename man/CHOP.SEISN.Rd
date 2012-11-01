\name{CHOP.SEISN}
\alias{CHOP.SEISN}

\title{CHOP SEISmic structure}
\description{
  Take a seismic structure and return a time limited version
}
\usage{
CHOP.SEISN(GH, sel = 1:4, WIN = NULL)
}

\arguments{
  \item{GH}{Seismic trace structure}
  \item{sel}{selection of traces}
  \item{WIN}{time window c(0,1)}
}

\value{
 Seismic trace structure
}

\author{Jonathan M. Lees<jonathan.lees.edu>}

\seealso{swig}
\examples{
data("GH")
sel <-  which(GH$COMPS=="V")

KF  <-  CHOP.SEISN(GH, sel=sel, WIN = c(0 , 5)  )

swig(KF)
}

\keyword{misc}
