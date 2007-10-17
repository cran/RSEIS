\name{winmark}
\alias{winmark}

\title{Window Mark}
\description{
 Add Mark up to current seismic trace with a bar desgnating a window selection.
}
\usage{
winmark(a1, a2, UD = 1, col = col)
}

\arguments{
  \item{a1}{x1-location}
  \item{a2}{x2-location }
  \item{UD}{UP-DOWN}
  \item{col}{color}
}
\details{
  Used for marking seismic traces
}
\value{
 Graphical Side Effect
}

\author{Jonathan M. Lees<jonathan.lees.edu>}
\examples{

plot(c(0,1), c(0,1))

winmark(.5, .6 ,col=4)


}

\keyword{aplot}
