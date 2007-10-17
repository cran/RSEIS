\name{Get1Dvel}
\alias{Get1Dvel}

\title{Read 1D velocity model}
\description{
  Read in a velocity model
}
\usage{
Get1Dvel(infile, PLOT = TRUE)
}

\arguments{
  \item{infile}{Path to ascii-text model}
  \item{PLOT}{logical, TRUE=plot}
}
\details{
  Reads Velocity model from a text file
}
\value{

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

\author{Jonathan M. Lees<jonathan.lees.edu>}

\examples{

\dontrun{ VELMOD1D = Get1Dvel("/home/lees/Progs/R_PAX/wu_coso.vel", PLOT=TRUE)}
\dontrun{
data(VELMOD1D)

Get1Dvel(VELMOD1D, PLOT=TRUE)
}
 }


\keyword{misc}
