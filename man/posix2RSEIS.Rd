\name{posix2RSEIS}
\alias{posix2RSEIS}
\title{Posix to RSEIS DATE/TIME
}
\description{Reformat posix time stamp to RSEIS list
}
\usage{
posix2RSEIS(p)
}
\arguments{
  \item{p}{posix time, either lt or ct
}
}

\value{returns a list of data/time in format RSEIS understands
}

\author{
Jonathan M. Lees<jonathan.lees@unc.edu>
}

\seealso{j2posix
}
\examples{
### make up a time:
P1 = as.POSIXct(Sys.time(), "America/New_York")  # in New York
R1 = posix2RSEIS(P1)
## also
unlist( as.POSIXlt(P1))  
}
\keyword{misc}
