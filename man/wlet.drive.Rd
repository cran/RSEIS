\name{wlet.drive}
\alias{wlet.drive}

\title{Interactive wavelet transform driver}
\description{
  interactive wavelet transform driver
}
\usage{
wlet.drive(Xamp, DT = 0.008, noctave = 6, nvoice = 20, w0=5, STAMP = NULL)
}

\arguments{
  \item{Xamp}{vector of signal}
  \item{DT}{sample interval (s) }
   \item{noctave}{number of octaves, default=6}
  \item{nvoice}{number of voices, nvoice = 20 }
  \item{w0}{central frequency for morlet wavelet, default=5}
  \item{STAMP}{character string for identification}
}

\value{
 Graphical Side Effects
}

\author{Jonathan M. Lees<jonathan.lees.edu>}

\seealso{RPGM, plotwlet, wlet.do }
\examples{


data(CE1)
plot(CE1$x, CE1$y, type='l')

if(interactive() ) wlet.drive(CE1$y, CE1$dt, STAMP=CE1$name)



}

\keyword{iplot}
