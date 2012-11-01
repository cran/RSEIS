\name{plotarrivals}
\Rdversion{1.1}
\alias{plotarrivals}
\title{plot theoretical arrival times for a seismic section
}
\description{plot theoretical arrival times for a seismic section
}
\usage{
plotarrivals(x, THEORY, add = FALSE)
}
\arguments{
  \item{x}{matrix of wiggles
}
  \item{THEORY}{theoretical arrivals
}
  \item{add}{logical, if TRUE=Add to existing plot
}
}
\details{plots go from top of page down
}
\value{graphical side effect
}

\author{
Jonathan M. Lees<jonathan.lees@unc.edu>
}
\note{Used for adding information to wiggle plots.
}


\seealso{symshot1, wiggleimage
}
\examples{

S1 <- symshot1()

wiggleimage(S1$smograms  , dt=(-S1$dt), dx=S1$dx)

plotarrivals(S1$x, S1$THEORY, add = TRUE)



}
\keyword{misc}
