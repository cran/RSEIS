\name{distseisnXY}
\alias{distseisnXY}
\title{Distances from an RSEIS list}
\description{
  Distances from an RSEIS list
}
\usage{
distseisnXY(GH, sta=list(nam="", x=0 , y=0 , z=0) , LOC=list(x=0, y=0 , z=0))
}
\arguments{
  \item{GH}{ Rseis list structure}
  \item{sta}{station list(x,y,z) }
  \item{LOC}{location list(x,y,z)  }
}

\value{
  list:
  \item{}{}
  \item{}{}
}

\author{Jonathan M. Lees<jonathan.lees@unc.edu>}
\note{
  Locations of stations and source should be projected.
}

\examples{

data(GH)
###   assume the lat lon in GH are x, y (projected)

staxy <-  list(nam=GH$stafile$name, x=GH$stafile$lon,
               y=GH$stafile$lat, z=GH$stafile$z)
 LOC <- list(x=GH$pickfile$LOC$lon, y=GH$pickfile$LOC$lat,
             z=GH$pickfile$LOC$z)
distseisnXY(GH, sta =staxy, LOC = LOC)



}
\keyword{misc}
