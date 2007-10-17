\name{tung.pulse}
\alias{tung.pulse}

\title{Volcanic Pulse Analysis}
\description{
  given a seiries of pulses, do analysis on each one
}
\usage{
tung.pulse(r, q, dt)
}

\arguments{
  \item{r}{x-coordinates}
  \item{q}{y-coordinates}
  \item{dt}{deltat, sample interval}
}
\details{
  Calculates, min, max of edges and center, then
  models the pulse with a triangular pulse and integegrates.
}
\value{
  vector=c(Ex[1], Ex[2], Ey[1], Ey[2], Cx, Cy, ar2, DefInt[1],
  DefInt[2], sum0)
  where:
  
  \item{Ex}{left minimum }
  \item{Ey}{ right  minimum}
  \item{ Cx, Cy}{ center (max?)}
  \item{ar2}{ area of triangle}
\item{DefInt[1]}{integral under curve}
\item{DefInt[2] }{integral under curve ( bottom triangle removed) }
\item{ sum0 }{ RMS amplitude}
}

\author{Jonathan M. Lees<jonathan.lees.edu>}

\seealso{peaks}
\examples{

\dontrun{
data(CE1)

ex = CE1$x[CE1$x>5.453291 &CE1$x< 5.507338]
why = CE1$y[CE1$x>5.453291 &CE1$x< 5.507338]
plot(ex, why, type='l')
tung.pulse(ex, why, CE1$dt)
}


}

\keyword{iplot}
