\name{Ray.time1D}
\alias{Ray.time1D}

\title{Seismic 1D Travel Time and raypath}
\description{
 Travel time and raypath from source to reciever in 1D local model.
}
\usage{Ray.time1D(indelta, inhpz, instaz, inlay, ztop, vel)
}

\arguments{
  \item{indelta}{distance in KM}
  \item{inhpz}{depth of hypocenter, km}
  \item{instaz}{elevation of station}
  \item{inlay}{number of layers}
  \item{ztop}{vector, tops of layers}
  \item{vel}{vector, velocities in layers}
}
\details{
 Uses local  1D velocity model, not appropriate for spherical earth.
}
\value{
  list:
\item{dtdr}{derivative of t w.r.t. horizontal distance}
\item{dtdz}{derivative of t w.r.t. z, depth}
\item{angle}{incidence angle, degrees}
\item{tt}{travel time, s}
\item{nnod}{number of nodes}
\item{znod}{node depths, km}
\item{rnod}{node offset distances, km}
     
}

\author{Jonathan M. Lees<jonathan.lees.edu>}

\seealso{travel.time1D,  Get1Dvel}
\examples{


data(VELMOD1D)

v <-  VELMOD1D
indelta=23.;
 inhpz=7.;
 instaz=0.;
nz = length(v$zp)

tees  <-  travel.time1D(indelta, inhpz, instaz, nz , v$zp , v$vp)
rays  <-  Ray.time1D(indelta, inhpz, instaz, nz , v$zp , v$vp)



plot(rays$rnod[1:rays$nnod] , -rays$znod[1:rays$nnod],type="n",
 xlab="distance, km" , ylab="Depth, km")

abline(h=-v$zp, lty=2, col=grey(0.80) )
points(rays$rnod[1:rays$nnod] , -rays$znod[1:rays$nnod], pch=8, col='green')
lines(rays$rnod[1:rays$nnod] , -rays$znod[1:rays$nnod])
points(rays$rnod[rays$nnod] , -rays$znod[rays$nnod], pch=6, col='red', cex=2)
#####  to coordinate this in space, need to rotate about
#####    the line between source and receiver locations

}

\keyword{misc}
