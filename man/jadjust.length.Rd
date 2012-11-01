\name{jadjust.length}
\alias{jadjust.length}

\title{Zero Padding}
\description{
  Add zeros to the end of the data if necessary so that  its length
     is a power of 2.  It returns the data with zeros added if nessary
     and the length of the adjusted data.
}
\usage{
jadjust.length(inputdata)
}

\arguments{
  \item{inputdata}{either a text file or an S object containing data }
}

\value{
 Zero-padded 1D array.

}
\references{See discussions in the text of "Practical Time-Frequency Analysis".}

\keyword{misc}
