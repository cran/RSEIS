\name{SELSTA}
\alias{SELSTA}
\title{Pick stations and components
  interactively
}
\description{Pick stations and components
  interactively.  This is a routine
  used in swig.
}
\usage{
SELSTA(GH, sel=1,  newdev = TRUE, STAY = FALSE)
}
\arguments{
  \item{GH}{RSEIS list
}

  \item{sel}{vector of index to selected traces
}


\item{newdev}{logical, whether
  to create a new device.
}
  \item{STAY}{logical, whether
  to keep device active.
}
}

\value{vector of index to list of stations and components
}

\author{Jonathan M. Lees<jonathan.lees@unc.edu>}

\seealso{swig}


\examples{
data(GH)

SELSTA(GH, sel=1:7 , newdev = TRUE, STAY = FALSE)

}
\keyword{misc}
