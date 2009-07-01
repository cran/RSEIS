\name{wiggleimage}
\Rdversion{1.1}
\alias{wiggleimage}
\title{Seismic section
}
\description{Plot a seismic section as shot record
}
\usage{
wiggleimage(Arot, dt = 1, dx = 1, col = "black")
}
\arguments{
  \item{Arot}{Matrix: columns are individual traces
}
  \item{dt}{Sample rate, seconds
}
\item{dx}{spacing in x-direction.  If a vector is given,
  it is used instead and dx is taken from the difference of the first
  to elements.
}
  \item{col}{color for plotting wiggles
}
}
\details{Plot is arranged with time going down the page
}
\value{Graphical side effects
}

\author{
Jonathan M. Lees<jonathan.lees@unc.edu>
}


\seealso{matsquiggle, varsquiggle
}
\examples{


S1 = symshot1()
wiggleimage(S1$smograms, dt = -S1$dt, dx = S1$x, col = "black")

}
\keyword{misc}
