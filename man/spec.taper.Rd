\name{spec.taper}
\alias{spec.taper}

\title{Taper spectrum}
\description{
 Taper function for spectrum analysis
}
\usage{
spec.taper(x, p = 0.1)
}

\arguments{
  \item{x}{time series trace}
  \item{p}{percent taper}
}
\details{
Cosine taper at ends of trace.
}
\value{
  tapered trace is returned.
}

\author{Jonathan M. Lees<jonathan.lees.edu>}

\examples{
data(CE1)
Xamp = CE1$y[CE1$x>5.443754 & CE1$x<5.615951]
###  10\% cosine taper:
xtap = spec.taper(Xamp, p = 0.1)


}

\keyword{misc}
