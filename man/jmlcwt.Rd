\name{jmlcwt}
\alias{jmlcwt}

\title{Wavelet Transform}
\description{
  wavelet transform from Rwave
}
\usage{
jmlcwt(input, noctave, nvoice = 1, w0 = 2 * pi, twoD = TRUE, plot = TRUE)
}

\arguments{
  \item{input}{signal}
  \item{noctave}{number of octives}
  \item{nvoice}{number of voices }
  \item{w0}{frequency}
  \item{twoD}{TRUE=2 dimensional}
  \item{plot}{ logical, TRUE=plot }
  
}
\details{
 See discussion in text of ``Practical Time-Frequency Analysis''.
}
\value{
 wavelet transform
}
\references{See discussion in text of ``Practical Time-Frequency Analysis''. }
\author{Rwave}
\note{ 
extracted this from Rwave because of non-desired side effects
}
\seealso{jmlcrc, jmlcfamily}

\keyword{misc}
