\name{ghstamp}
\alias{ghstamp}
\title{Identification stamp for RSEIS data
}
\description{Prepare a character string stamp for
  idenitication of plots of of signals in swig.
}
\usage{
ghstamp(GH, sel, WIN = c(485, 600))
}
\arguments{
  \item{GH}{RSEIS list structure
}
  \item{sel}{numeric index vector, selection of traces
}
  \item{WIN}{time window within a trace
}
}
\details{The character string can be used as
  a stamp on plots for unique identification.  Uses
  the info list in the RSEIS list.  This function combines Zdate
  with the window time information.
}
\value{character array for each component in the sel vector.
}

\author{
Jonathan M. Lees<jonathan.lees@unc.edu>
}


\seealso{Zdate, MTM.drive, plotwlet
}


\examples{
data(KH)
ghstamp(KH)

data(GH)
ghstamp(GH, sel=1:3)


}
\keyword{misc}
