\name{dowiggles}
\Rdversion{1.1}
\alias{dowiggles}
\title{Plot wiggles
}
\description{Plot wiggles
}
\usage{
dowiggles(AMAT, dt, dx)
}
\arguments{
  \item{AMAT}{Matrix of seismic time series
}
  \item{dt}{time interval, sec
}
  \item{dx}{x-spacing
}
}

\value{graphical side effects
}

\author{
Jonathan M. Lees<jonathan.lees@unc.edu>
}

\seealso{wiggleimage, matsquiggle
}
\examples{
S1 = symshot1()

dowiggles(S1$smograms,S1$dt, S1$x)



}
\keyword{misc}
