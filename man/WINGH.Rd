\name{WINGH}
\alias{WINGH}
\title{Window a GH structure and extract data
}
\description{
  Window a time slice of seismic data and extract from
  a GH structure.
}
\usage{
WINGH(GH, sel = 1, WIN = c(0,1) )
}
\arguments{
  \item{GH}{RSEIS seismic list
}
  \item{sel}{Select which traces to extract
}
\item{WIN}{Time window to extract (seconds from the beginning
  of the first trace.)
}
}
\details{Preserves the data structure of the GH list.
  The purpose of this function is to extract a small subset
  of data from a larger data set (or longer time series)
  for subsequent processing.
}
\value{New GH structure.
}

\author{
Jonathan M. Lees<jonathan.lees@unc.edu>
}


\seealso{swig
}
\examples{

if(interactive()){
data(GH)

swig(GH, sel=which(GH$COMPS=="V" ))


jh = WINGH(GH, sel = which(GH$COMPS=="V" ), WIN = c(3.821281, 12.861820) )

swig(jh)
## compare with:
swig(GH, sel=which(GH$COMPS=="V" ), WIN = c(3.821281, 12.861820))



}
}
\keyword{misc}
