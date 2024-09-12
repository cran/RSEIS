\name{SEIS2list}
\alias{SEIS2list}
\title{Convert a SEIS list to a list of seismograms
}
\description{
  Convert a SEIS list to a list of seismograms each
  independent.
}
\usage{
SEIS2list(GH)
}
\arguments{
  \item{GH}{SEIS list (swig input)
}
}
\details{
  The list returned is useful for editing
  or modifying the seismic data prior to swig.
}
\value{List of seismograms.
}

\author{
Jonathan M. Lees<jonathan.lees@unc.edu>
}


\seealso{plotGH, swig
}
\examples{

data(GH)
gg = SEIS2list(GH)
##   for(i in 1:length(gg) )
i = 1

{
plotGH(gg[[i]]); Sys.sleep(0.2) 
}


}
\keyword{misc}
