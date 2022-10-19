\name{STALTA}
\alias{STALTA}
\title{Short term, long term  average ratio
}
\description{
  Calculate the short term, long term average ratios of the squared
  amplitude in a time series.
}
\usage{
STALTA(y, fwlen = 125, bwlen = 125)
}
\arguments{
  \item{y}{ vector, or time series
}
  \item{fwlen}{forward number of samples 
}
  \item{bwlen}{backward number of samples 
}
}
\details{Calculates the ratio of the forward/backard mean square sum.
}
\value{vector of ratios
}

\author{
Jonathan M. Lees<jonathan.lees@unc.edu>
}
\note{
  All filtering or pre and post analysis should be done outside of ratio
  curve estimate.  
}


\seealso{STLTcurve, PSTLTcurve
}
\examples{
### easy example find P and S-wave arrivals, low noise
data(GH)
i = 6
z = GH$JSTR[[i]]

z.curve = STALTA(z, fwlen = 10, bwlen = 325)

ex = seq(from=0, length=length(z), by=GH$dt[i])
par(mfrow=c(2, 1) )
plot(ex, z, type='l')
plot(ex, z.curve, type = 'l' )

aa = peaks(z.curve, span = 11, do.pad = TRUE)
wa = which( aa & z.curve>50 )

abline(v=wa*GH$dt[i]  , col='red')
par(mfg=c(1,1) )
abline(v=wa*GH$dt[i]  , col='red')



}
\keyword{misc}
