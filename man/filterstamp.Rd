\name{filterstamp}
\alias{filterstamp}
\title{Make  Filter Stamp}
\description{
  Create an text stamp describing a filter
}

\usage{
filterstamp(fl=1/2, fh=10, type="BP")
}
\arguments{
  \item{fl}{vector, low frequency}
  \item{fh}{vector,high frequency}
  \item{type}{vector,type of filter}
}
\details{
  If the frequency is less than 1, the period is displayed.
  For now only 3 digits are displayed.  If the first argument, fl, is a
  list the parameters are extracted from the list and the other arguments
  are ignored.
}
\value{
  \item{stamps}{text strings}
}

\author{Jonathan M. Lees<jonathan.lees@unc.edu>}

\seealso{FILT.spread}
\examples{

fl = c(0.01, 2)
fh = c(10, 20)
type = "BP"
filterstamp(fl, fh, type)

FILT=list(ON=TRUE, fl=1/2, fh=12, type="HP", proto="BU")
filterstamp(FILT)

FILT=list(ON=TRUE, fl=1/2, fh=12, type="BP", proto="BU")
filterstamp(FILT)

FILT=list(ON=TRUE, fl=1/2, fh=12, type="LP", proto="BU")
filterstamp(FILT)



}
\keyword{misc}
