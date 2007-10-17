\name{hilow}
\alias{hilow}

\title{Find Maxima and Minima}
\description{
  Search for Extrema along time series
}
\usage{
hilow(y)
}

\arguments{
  \item{y}{time series}
}

\value{

 LIST:
  \item{hi}{indexes to peaks}
  \item{lo}{indexes to valleys}
  
}

\examples{
ex = seq(from=0, to=4*pi, length = 200)

y = sin(ex)
plot(ex, y, type='l')

peakval = hilow(y)

abline(v=ex[peakval$hi], col='green')
abline(v=ex[peakval$lo], col='red')


}


\author{Jonathan M. Lees<jonathan.lees.edu>}

\seealso{peaks}

\keyword{misc}
