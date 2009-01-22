\name{varsquig}
\alias{varsquig}
\title{Var-Squiggle plot}
\description{
  Plot one seismogram in Var-Squiggle mode - like on an exploration record
  section with half the wiggled shaded.
}
\usage{
varsquig(x, y, L = locator(2), FLIP = FALSE, col = "blue", var = 0)
}
\arguments{
  \item{x}{X (time axis) coordinates}
  \item{y}{Y amplitudes}
  \item{L}{rectangular region on plot where plotting occurs}
  \item{FLIP}{logical - whether to flip the amplitudes by -1}
  \item{col}{color for shading}
  \item{var}{logical, whether to shade}
}
\details{
  A set of traces can be plotted after the plotting region has been set.
}
\value{
 Graphical Side Effects
}

\author{Jonathan M. Lees<jonathan.lees@unc.edu>}
\note{
  varsquig is meant to be used within other program not as a stand alone
  routine.  The plotting region must be set up prior to plotting.  The
  time series is scaled to fitt in the rectangular region defined by L.

  
}
\seealso{varsquiggle}
\examples{

data(KH)

x = KH$ex[KH$ex>95& KH$ex<125]
y = KH$JSTR[[1]][KH$ex>95& KH$ex<125]


plot(x , y , type='l')


u = par('usr')
L = list(x=c(u[1], u[2]), y = c(u[3], u[4]))

plot(L$x, L$y, type='n')
 varsquig(x, y, L=L , FLIP=FALSE, col="blue", var=TRUE)

 varsquig(x, y, L=L , FLIP=FALSE, col="black", var=FALSE)



}
\keyword{hplot}
