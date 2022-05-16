\name{combineSEIS}
\alias{combineSEIS}
\title{Combine SEIS lists
}
\description{Combine 2 SEIS format lists into one list suitable for swig.
}
\usage{
combineSEIS(IH, IV)
}
\arguments{
  \item{IH}{SEIS list (swig input)
}
  \item{IV}{SEIS list (swig input)
}
}
\details{
  This will take two SEIS lists and merge them into one.
}
\value{SEIS list suitable for swig.
}

\author{
Jonathan M. Lees<jonathan.lees@unc.edu>
}


\seealso{swig, Mine.seis, prepSEIS
}
\examples{
\dontrun{
#######  say you have 2 databases - extract from each:
GH = Mine.seis(at1, at2, DB1, grepsta, grepcomp, kind = -1)
JH = Mine.seis(at1, at2, DB2, grepsta, grepcomp, kind = -1)
#### merge the 2 structures
BH = combineSEIS(GH, JH)
###### plot and interact
swig(BH)

}


}
\keyword{misc}
