\name{VELMOD1D}
\alias{VELMOD1D}
\docType{data}
\title{Sample Velocity Model}
\description{
 Seismic Velocity Model for Coso California
}
\usage{data(VELMOD1D)}
\format{
 LIST:
  \item{zp}{vector of Tops of Layers, P-wave, (km)}
  \item{vp}{vector of velocities of Layers, P-wave,(km/s)}
   \item{ep}{errors for velocities, P-wave,(km/s) }
   \item{zs}{vector of Tops of Layers, S-wave, (km)}
  \item{vs}{vector of velocities of Layers, S-wave,(km/s)}
   \item{es}{errors for velocities, S-wave,(km/s) }
 \item{name}{character, name of model }
 \item{descriptor}{character vector description of model }


}
\details{
 Velocity model from a text file

}

\references{
 Wu, H., and J. M. Lees (1999), Three-dimensional P- and S-wave velocity structures of the Coso Geothermal Area, California, from microseismic traveltime data, J. Geophys. Res. 104, 13,217-13,233.


}
\examples{
data(VELMOD1D)
Get1Dvel(VELMOD1D, PLOT=TRUE)

## maybe str(VELMOD1D) ; plot(VELMOD1D) ...
}
\keyword{datasets}
