\name{FRWDft}
\alias{FRWDft}

\title{Forward fourier Transform}
\description{
  Forward fourier Transform
}
\usage{
FRWDft(g, n, tstart, dt)
}

\arguments{
  \item{g}{input signal}
  \item{n}{number of points}
  \item{tstart}{start of trace }
  \item{dt}{sample interval, s }
}

\value{
   \item{G}{fourier compnents}
   \item{f}{frequency vector}
  \item{t}{time vector}

}

\author{Jonathan M. Lees<jonathan.lees.edu>}

\seealso{INVRft}
\examples{
zil  <-  rnorm(300)
fss  <-  FRWDft( zil, length(zil), 0, 0.004)   




}

\keyword{misc}
