\name{CE1}
\alias{CE1}
\docType{data}
\title{Single Seismogram}
\description{
Single Seismogram from Coso California
}
\usage{data(CE1)}
\format{
 list(x=0,  y=0,     dt=0,      name ="",   Tpick=0,   mark ="",  deltat=0)

}

\references{
 Lees, J.M., 2004. Scattering from a fault interface in the Coso geothermal field. Journal of Volcanology and Geothermal Research, 130(1-2):
61-75.
}
\examples{
data(CE1)
plot(CE1$x, CE1$y, type='l')

## maybe str(CE1) ; plot(CE1) ...
}
\keyword{datasets}
