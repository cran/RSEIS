\name{wlet.drive}
\alias{wlet.drive}

\title{Interactive wavelet transform driver}
\description{
  interactive wavelet transform driver
}
\usage{
wlet.drive(Xamp, DT = 0.008, STAMP = NULL)
}

\arguments{
  \item{Xamp}{vector of signal}
  \item{DT}{sample interval (s) }
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
