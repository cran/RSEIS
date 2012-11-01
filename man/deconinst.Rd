\name{deconinst}
\alias{deconinst}

\title{Deconvolve instrument response from seismic data}
\description{
 Deconvolve instrument response from seismic data
}
\usage{
deconinst(data, sintr, KAL, key, Calibnew, waterlevel = 1e-08)
}

\arguments{
  \item{data}{Real vector of data}
  \item{sintr}{sample interval }
  \item{KAL}{Kalibrated response list }
  \item{key}{number of instrument }
  \item{Calibnew}{new instrument, complex vector or }
  \item{waterlevel}{waterlevel for low frequency division }
}
\details{
   To avoid problems with dividing by very small numbers,
  water level is set =1.e-8
}
\value{
 deconvolved signal
}

\author{Jonathan M. Lees<jonathan.lees.edu>}
\note{
  Calibnew(1)==3 then use a cos (hanning) taper

}
\seealso{PreSet.Instr, ReadSet.Instr, INSTresponse}
\examples{


Kal <- PreSet.Instr()
amp <- rnorm(1024)
Calibnew <- c(1,1.0, 0.0 )

dy  <- deconinst(amp, 0.008, Kal,1, Calibnew, waterlevel=1.e-8)



}

\keyword{misc}
