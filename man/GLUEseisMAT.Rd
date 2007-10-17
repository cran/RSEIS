\name{GLUEseisMAT}
\alias{GLUEseisMAT}

\title{GLUEseisMAT}
\description{
 Find duplicated stations in a matrix and fill in the traces that are continuations,
	return the new matrix and the vector duplicates

}
\usage{
GLUEseisMAT(GFIL)
}

\arguments{
  \item{GFIL}{list of data and headers, with duplicated stations glued}
}

\value{
 New List of data and headers with same sensors/components glued together
}

\author{Jonathan M. Lees<jonathan.lees.edu>}

\seealso{Mine.seis}

\keyword{misc}
