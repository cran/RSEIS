\name{detail.pick}
\alias{detail.pick}

\title{Detail Pick on 3-component seismogram}
\description{
  Pops up three components and prepares menu items for picking
}
\usage{
detail.pick(y, ex, dt, TIT = "")
}

\arguments{
  \item{y}{signal amplitudes}
  \item{ex}{x-axis }
  \item{dt}{deltaT, sample rate, s}
  \item{TIT}{title}
}
\details{
  Creates interactive session for picking seismograms.  Is called from swig.
}
\value{
  KSAVE = list(x=xsave, y=ysave)
}

\author{Jonathan M. Lees<jonathan.lees.edu>}

\seealso{swig}
\examples{

data(CE1)

  detail.pick(CE1$y, CE1$x, CE1$dt, TIT = "")
   

}

\keyword{misc}
\keyword{iplot}