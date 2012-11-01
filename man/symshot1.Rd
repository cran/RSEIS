\name{symshot1}
\alias{symshot1}

\title{Simulate a seismic shot}
\description{
  Simulate an exploration style seismic shot with ground roll, air wave,
  refractions and reflections.
}
\usage{
symshot1(PLOT = FALSE)
}
\arguments{
  \item{PLOT}{logical, TRUE=plot the wiggles.  DEFAULT=FALSE
}

  }
\details{
  Arrivals are calculated based on geometric
  considerations with a 1D layered model.
}
\value{

\item{ smograms}{Matrix: columns are individual traces}
\item{ dt}{sample interval in time, s}
\item{ x}{x locations}
\item{dx }{spacing in X-direction}
\item{REFL }{reflection information}
\item{REFR }{refraction image}
\item{ GRrec}{ground roll image}
\item{AIRrec  }{air wave image}
\item{THEORY }{List of theoretical values}
\item{trefrac }{refraction arrival times}
\item{treflex }{reflection arrival times}

\item{tair }{Air arrival times}
\item{velair }{velocity for the air wave}
\item{ mod}{Layered Model}


}

\references{Sherrif}
\author{Jonathan M. Lees<jonathan.lees@unc.edu>}
\note{MOdel is relatively simple:
}
\seealso{wiggleimage, symshot}
\examples{

S1 <- symshot1()
wiggleimage(S1$smograms, dt = -S1$dt, dx = S1$x, col = "black")


}

\keyword{misc}
