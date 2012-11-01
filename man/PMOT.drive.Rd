\name{PMOT.drive}
\alias{PMOT.drive}

\title{Interactive Particle Motion Plot}
\description{
Plot Hodogram and show seismic particle motion
}
\usage{
PMOT.drive(temp, dt, pmolabs = c("Vertical", "North", "East"), STAMP = "", baz = 0)
}

\arguments{
  \item{temp}{matrix of 3-component seismic signal}
  \item{dt}{sample interval (delta-T, seconds)}
  \item{pmolabs}{labels for traces}
  \item{STAMP}{Character string Identification stamp}
  \item{baz}{Back Azimuth, degrees}
  
}
\details{
 Input matrix should V, N, E.  Baz is not implemented yet.
}
\value{
 Graphical Side Effect.
}

\author{Jonathan M. Lees<jonathan.lees.edu>}


\examples{

data("GH")
sel <-  which(GH$STNS == "CE1")


YMAT  <-  cbind(GH$JSTR[[sel[1]]][1168:1500],
GH$JSTR[[sel[2]]][1168:1500],
GH$JSTR[[sel[3]]][1168:1500])

dt  <-   GH$dt[ sel[1] ]
ftime  <-  Zdate(GH$info, sel[1], 1)

\dontrun{
PMOT.drive(YMAT, dt, pmolabs = c("Vertical", "North", "East"),
STAMP =ftime )

}


}

\keyword{misc}
\keyword{iplot}