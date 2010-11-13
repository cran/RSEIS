\name{save.wpix}
\alias{save.wpix}
\title{Save WPIX from swig output}
\description{Save WPIX from swig output
}
\usage{
save.wpix(KOUT, fn = "wpix.out")
}
\arguments{
  \item{KOUT}{List output from swig}
  \item{fn}{file name for saving.}
}
\details{
  Takes the output list from swig, specifically the WPX
  component (KOUT\$WPX) and writes a table to the file system.
  This function is embedded in view.seis.
}
\value{
  Side effects: file is created and appended to.
}
\author{Jonathan M. Lees<jonathan.lees@unc.edu>}
\note{
  User must have write permission to the file.
}

\seealso{view.seis, swig}

\keyword{misc}
