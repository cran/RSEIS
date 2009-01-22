\name{detrend}
\alias{detrend}

\title{Remove trend from time series signal}
\description{
 Remove trend from time series signal
}
\usage{
detrend(x)
}

\arguments{
  \item{x}{vector}
}
\details{
 Removes the trend from a signal.
}
\value{
 vector with linear trend removed.
}

\author{Jonathan M. Lees<jonathan.lees@unc.edu>}

\seealso{mean}
\examples{

dt = 0.001

 t = seq(0, 6, by=0.001)


y = 5*sin(2*pi*10*t)

plot(t,y, type='l')

y = y  + 3 * t
plot(t,y, type='l')


dy = detrend(y)

plot(t,dy, type='l')

}

\keyword{misc}
