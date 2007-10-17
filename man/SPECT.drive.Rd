\name{SPECT.drive}
\alias{SPECT.drive}

\title{Interactive Spectrogram Driver}
\description{
  Interactive Spectrogram Driver
}
\usage{
SPECT.drive(Xamp, DT = 0.008, NEW = TRUE, STAMP = NULL)
}

\arguments{
  \item{Xamp}{signal trace}
  \item{DT}{deltaT sample interval, s}
  \item{NEW}{logical, TRUE=recalculate spectrum}
  \item{STAMP}{character stamp for identification}
}
\details{
  Interactive buttons are set internally.
 }
\value{
Graphical Side Effects
}

\author{Jonathan M. Lees<jonathan.lees.edu>}

\seealso{plotevol, RPMG}
\examples{
data(CE1)

#########  Xamp = CE1$y[CE1$x>5.443754 & CE1$x<5.615951]

Xamp = CE1$y
plot(Xamp, type='l')


DT = CE1$dt

SPECT.drive(Xamp, DT = DT, NEW = TRUE, STAMP = NULL)


}

\keyword{iplot}
