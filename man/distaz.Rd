\name{distaz}
\alias{distaz}

\title{Distance-Azimuth from event source to receiver station}
\description{
  Calculates the distance in km, degrees and the azimuth and back
  azimuth from station to source
}
\usage{
distaz(olat, olon, tlat, tlon)
}

\arguments{
  \item{olat}{Source (origin) latitude}
  \item{olon}{Source (origin) longitude}
  \item{tlat}{Station (receiver) latitude}
  \item{tlon}{Station (receiver) longitude}
}

\value{
  \item{del}{Delta (degrees)}
  \item{az}{azimuth, degrees}
  \item{baz}{back-azimuth, degrees}
  \item{dist}{distance, km}
  \item{err}{ error flag, 0=failure, 1=success }
  
}

\author{Jonathan M. Lees<jonathan.lees@unc.edu>}
\note{

  Earth radius is fixed at 6378.2064 km.
  
  
}
\seealso{GreatDist}
\examples{

london = c(51.53333, -0.08333333)
     santiago = c(-33.46667, -70.75)

     distaz(london[2], london[1], santiago[2], santiago[1])




}

\keyword{misc}
