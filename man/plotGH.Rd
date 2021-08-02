\name{plotGH}
\alias{plotGH}
\title{Plot a seismic trace.
}
\description{
Quick and dirty plot of a seismic trace as recorded and
save using stream2GHnosens or other RSEIS savers.
}
\usage{
plotGH(h)
}
\arguments{
  \item{h}{
  This is a standard GH object as defined in RSEIS
}
}
\details{
The code converts the amp component to a ts object.
}
\value{
Side effects
}

\author{
Jonathan M. Lees<jonathan.lees@unc.edu>
}

\seealso{
RSEIS::prepSEIS, RSEIS::prep1wig, RSEIS::PLOT.SEISN, RSEIS::swig
}
\examples{
\dontrun{
zh = JGET.seis(file='file.RDATA', kind=-1)
plotGH(zh)

}



}
\keyword{misc}
