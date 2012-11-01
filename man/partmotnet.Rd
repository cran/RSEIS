\name{partmotnet}
\alias{partmotnet}

\title{Particle Motion on Stereonet}
\description{
  Show Particle Motion on Stereonet
}
\usage{
partmotnet(temp, LINES = FALSE, STAMP = STAMP, COL = rainbow(100))
}

\arguments{
  \item{temp}{matrix of 3-component seismic data}
  \item{LINES}{logical, TRUE=draw lines}
  \item{STAMP}{identification stamp}
  \item{COL}{color palette}
}
\details{
Show seismic particle motion on a sphere color coded by time.
}
\value{
  graphical side effect
}

\author{Jonathan M. Lees<jonathan.lees.edu>}

\examples{


data("GH")

temp = list(x=GH$JSTR[[1]][1168:1500],
y=GH$JSTR[[2]][1168:1500], z=GH$JSTR[[3]][1168:1500])

sx = partmotnet(temp, STAMP="Example",
LINES=TRUE, COL=rainbow(100) )



}

\keyword{hplot}
