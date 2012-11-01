\name{SNET.drive}
\alias{SNET.drive}

\title{stereonet representation of particle motion}
\description{
 stereonet representation of particle motion
}
\usage{
SNET.drive(intempmat, pmolabs = c("Vertical", "North", "East"), STAMP = "")
}

\arguments{
  \item{intempmat}{matrix of 3-component seismogram}
  \item{pmolabs}{labels for components}
  \item{STAMP}{Identification stamp }
}
\details{
 Interactive driver for partmotnet.
}
\value{
  Graphical Side effect
}

\author{Jonathan M. Lees<jonathan.lees.edu>}

\seealso{partmotnet}
\examples{


data("GH")

temp  <-  cbind(GH$JSTR[[1]], GH$JSTR[[2]], GH$JSTR[[3]])

atemp  <- temp[1168:1500, ]
SNET.drive(atemp, pmolabs = c("Vertical", "North", "East"), STAMP = "")

}

\keyword{hplot}
\keyword{iplot}
