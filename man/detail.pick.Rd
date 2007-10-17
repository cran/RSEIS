\name{detail.pick}
\alias{detail.pick}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Detail Pick on 3-component seismogram}
\description{
  Pops up three components and prepares menu items for picking
}
\usage{
detail.pick(y, ex, dt, TIT = "")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{y}{signal amplitudes}
  \item{ex}{x-axis }
  \item{dt}{deltaT, sample rate, s}
  \item{TIT}{title}
}
\details{
  Creates interactive session for picking seismograms.  Is called from PICK.GEN.
}
\value{
  KSAVE = list(x=xsave, y=ysave)
}

\author{Jonathan M. Lees<jonathan.lees.edu>}

\seealso{PICK.GEN}
\examples{

data(CE1)

  detail.pick(CE1$y, CE1$x, CE1$dt, TIT = "")
   

}

\keyword{misc}
\keyword{iplot}