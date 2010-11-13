\name{dogetsac}
\alias{dogetsac}
\title{Get SAC files}
\description{Get a set of SAC files given file names
}
\usage{
dogetsac(fnames)
}
\arguments{
  \item{fnames}{vector of file names}
}
\details{
   Program uses C-code to read in binary SAC files
  and returns the header information and the time series data.
}
\value{
  RSEIS list

}

\author{Jonathan M. Lees<jonathan.lees@unc.edu>}
\note{
Sometimes information in the SAC header is omitted by the user
  and instead stored in the file name.  In that case the
  station name and component name will have to be replaced
  by the appropriate string from the file name.

  
}
\seealso{ZGET.sac, swig, JGET.seis, Package:Rsac}
\examples{

\dontrun{
fnames=list.files("20020122", pattern="SAC", full.names=TRUE)

ZH = dogetsac(fnames)
swig(ZH)


}
}
\keyword{misc}
