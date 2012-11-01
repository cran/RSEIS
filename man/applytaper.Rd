\name{applytaper}
\alias{applytaper}

\title{Apply taper to seismic trace}
\description{
 Apply taper to ends of a time series for spectrum analysis.
}
\usage{
applytaper(f, p = 0.05)
}

\arguments{
  \item{f}{signal}
  \item{p}{percent taper}
}
\details{
 10 percent taper is 5 percent on each end.
}
\value{
 Tapered time series.
}

\author{Jonathan M. Lees<jonathan.lees.edu>}


\examples{
data(CE1)
Xamp  <-  CE1$y[CE1$x>5.443754 & CE1$x<5.615951]
Tamp  <-  applytaper(Xamp, p = 0.05)
}

\keyword{misc}
