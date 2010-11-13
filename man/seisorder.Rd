\name{seisorder}
\alias{seisorder}
\title{Order seismic traces}
\description{
  Use RSEIS structure to get convenient ordering of seismic data
}
\usage{
seisorder(GH, ORD, VNE = c("V", "N", "E"))
}
\arguments{
  \item{GH}{RSEIS list}
  \item{ORD}{predetermined ordering, list(name, dist)}
  \item{VNE}{Order, for components, default=c("V", "N", "E") }
}
\details{
  Uses information aboutthe location of the stations
  to determine appropriate order.  Order can be determined from the
  location of the stations, or from the travel times.
  
}
\value{
  Vector of indeces of GH in correct order
}

\author{Jonathan M. Lees<jonathan.lees@unc.edu>}
\note{
  If ORD is provided from travel times, it uses this instead
}
\seealso{JGET.seis}
\examples{

data(GH)
staf = GH$stafile

################   get the distances from the source to the stations
d1  = GreatDist(GH$pickfile$LOC$lon, GH$pickfile$LOC$lat,staf$lon, staf$lat)

###  staf has the names of the stations already, so insert the order via
###                       dist
staf$dist=d1$dkm

sorder = seisorder(GH, staf, VNE= c("V", "N", "E"))

\dontrun{
swig(GH, sel=sorder)
}

}
\keyword{misc}
