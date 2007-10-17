\name{jmlcfamily}
\alias{jmlcfamily}
\title{Ridge Chaining Procedure}
\description{
  Chains the ridge estimates produced by the function 'crc'.

}
\usage{
jmlcfamily(ccridge, bstep = 1, nbchain = 100, ptile = 0.05)
}

\arguments{
  \item{ccridge}{unchained ridge set as the output of the function 'crc'}
  \item{bstep}{maximal length for a gap in a ridge.}
  \item{nbchain}{maximal number of chains produced by the function.}
  \item{ptile}{relative threshold for the ridges.}
}
\details{
  'crc' returns a measure in time-frequency (or time-scale) space.
     'cfamily' turns it into a series of one-dimensional objects
     (ridges). The measure is first thresholded, with a relative
     threshold value set to the input parameter ptile. During the
     chaining procedure, gaps within a given ridge are allowed and
     filled in. The maximal length of such gaps is the input parameter
     bstep.

   }


 
     
\value{
  Returns the results of the chaining algorithm

}

\author{Ridge Chaining Procedure}
\note{
  taken from Rwave package. this was extracted from Rwave to reduce other side effects
}
\seealso{crc}

\keyword{misc}
