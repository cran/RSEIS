\name{fixNA}
\alias{fixNA}
\title{Fix NA values.
}
\description{Replace NA values in a time series
  with mean values between end points of missing segments,
  or first and last real values in case the NA's are at the beginning or ends of traces.
}
\usage{
fixNA(y)
}
\arguments{
  \item{y}{numeric vector
}
}
\details{
  fixNA searches for stretches of NA 's in a time series
  and replaces the NA values with numeric values based ont he two end
  points of each section.
}
\value{numeric vector with no NA values.
}

\author{
Jonathan M. Lees<jonathan.lees@unc.edu>
}

\note{function is used primarily in filter applications.
}


\seealso{butfilt
}
\examples{

## source("~/Site/TA_DATA/CODE/fixNA.R")

### last samples are NA
zig = rnorm(25)
zig[10:15] = NA

noNA = fixNA(zig)

### first samples are NA
zig = rnorm(25)
zig[1:5] = NA
noNA = fixNA(zig)


zig = rnorm(25)
zig[1:5] = NA
zig[21:25] = NA

noNA = fixNA(zig)



zig = rnorm(25)
zig[1] = NA
zig[21:25] = NA
zig[10:12] = NA

noNA = fixNA(zig)
cbind(zig, noNA)




}
\keyword{misc}
