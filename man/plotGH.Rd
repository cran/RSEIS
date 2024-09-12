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
  The input is a list that has, as a minimum the following items:
  'amp', 'dt', 'sta', 'comp', 'DATTIM'.  
  Item 'amp', a time series vector is converted to a ts object. 
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

data(GH)
L1 = length(GH$JSTR)
DD = data.frame(GH$info)
####  convert to individual traces,
###  here just use the first one:
i = 1
AA = DD[i,]
zh  = list(fn = AA$fn, sta =GH$STNS[i] , comp = GH$COMP[i], 
            dt = AA$dt, DATTIM = AA, N = AA$n1, units = NA, 
            coords = NA, amp = GH$JSTR[[i]]  )
######  plot 
plotGH(zh)


}
\keyword{misc}
