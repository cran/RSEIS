\name{selpgen}
\alias{selpgen}
\title{Pick stations and components
  interactively
}
\description{Pick stations and components
  interactively.  This is a routine
  used in swig.
}
\usage{
selpgen(MH, newdev = TRUE, STAY = FALSE)
}
\arguments{
  \item{MH}{RSEIS list
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

\author{
Jonathan M. Lees<jonathan.lees@unc.edu>
}


\seealso{swig
}

\keyword{misc}
