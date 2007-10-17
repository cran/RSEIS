\name{getb1b2}
\alias{getb1b2}

\title{Event Detection}
\description{
Used for event detection
}
\usage{
getb1b2(J, L, zwin, maxx, max2)
}

\arguments{
  \item{J}{Thresh.J}
  \item{L}{Thresh.J}
  \item{zwin}{maximum of forwd and bakwrd windows}
  \item{maxx}{max number of points}
  \item{max2}{all points}
}

\value{
  vector c(b1,b2) 
}

\author{Jonathan M. Lees<jonathan.lees.edu>}
\note{ Used for thresholding on event detection.
}
\seealso{Thresh.J, ETECTG}

\keyword{misc}