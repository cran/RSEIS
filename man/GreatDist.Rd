\name{GreatDist}
\alias{GreatDist}

\title{Distance Along Great Circle Arc}
\description{
 Distance Along Great Circle Arc in degrees, kilometers
}
\usage{
GreatDist(LON1, LAT1, LON2, LAT2, EARTHRAD= 6371)
}

\arguments{
  \item{LON1}{Longitude, point1}
  \item{LAT1}{Latitude, point1}
  \item{LON2}{Longitude, point2}
  \item{LAT2}{Latitude, point2}
  \item{EARTHRAD}{optional earth radius,  default  = 6371}
  
}

\value{
  LIST:
  \item{drad}{distance in radians}
  \item{ddeg}{distance in degrees}
  \item{dkm}{distance in kilometers}
 
}

\author{Jonathan M. Lees <jonathan.lees@unc.edu>}

\examples{

###  get distance between London, England and Santiago, Chile
london  <-  c(51.53333, -0.08333333)
santiago  <-  c(-33.46667, -70.75)

GreatDist(london[2], london[1], santiago[2], santiago[1])

}

\keyword{misc}
