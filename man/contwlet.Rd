\name{contwlet}
\alias{contwlet}

\title{Contour Wavelet Transform}
\description{
 Contour Wavelet Transform
}
\usage{
contwlet(baha, Ysig, dt, clev = 0.75, NLEV = 12,
 zscale = 1, zbound = NULL, col = col, ygrid = FALSE,
 WUNITS = "Volts", PEAX = NULL)
}

\arguments{
  \item{baha}{Output of wavelet transform (image)}
  \item{Ysig}{input signal to wavelet transform}
  \item{dt}{DeltaT, sample rate }
  \item{clev}{levels for contours}
  \item{NLEV}{number of levels }
  \item{zscale}{scale of amplitudes }
  \item{zbound}{bounds for scale of interest}
  \item{col}{color for contour lines}
  \item{ygrid}{logical, TRUE=add grid lines}
  \item{WUNITS}{Units of wavelet transform}
  \item{PEAX}{peaks structure}
}

\value{
  Graphical Side Effect
}

\author{Jonathan M. Lees<jonathan.lees.edu>}

\seealso{plotwlet, wlet.do, wlet.drive}

\keyword{hplot}
\keyword{aplot}