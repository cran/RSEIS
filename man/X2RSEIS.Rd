\name{X2RSEIS}
\alias{X2RSEIS}
\title{Extract data to RSEIS file
}
\description{swig Button Extract seismic data in RSEIS
  and save in GH format for exchange.
}
\usage{
X2RSEIS(nh, g)
}
\arguments{
  \item{nh}{RSEIS seismic data format
}
  \item{g}{swig parameters
}
}
\details{This function is used internally in
  RSEIS as a button in swig.  The program should be run in
  a directory that has write permission.

  The data is saved as  a GH list.
}
\value{No value, writes to disk
}

\author{
Jonathan M. Lees<jonathan.lees@unc.edu>
}
\seealso{XTR, X2SAC, swig
}
\examples{
\dontrun{
###  get data:
GH <-  Mine.seis(at1, at2, DB, NULL , NULL,
        kind = 1, Iendian=1)
w <- swig(GH, PADDLAB=c("X2SAC","X2RSEIS","YPIX" ) )

}
}
\keyword{misc}
