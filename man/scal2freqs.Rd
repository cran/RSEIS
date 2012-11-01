\name{scal2freqs}
\alias{scal2freqs}

\title{Wavelet Frequency Scale}
\description{
 Get frequencies associated with the wavelet transform.
}
\usage{
scal2freqs(octs, dt, plot = FALSE)
}

\arguments{
  \item{octs}{number of octaves}
  \item{dt}{ sample rate, s }
  \item{plot}{logical, TRUE=plot}
}
\details{
  Use morelet wavelet to estimate frequency from
  wavelet transform.
}
\value{
 frequency values
}

\author{Jonathan M. Lees<jonathan.lees.edu>}

\seealso{Mmorlet, fft}
\examples{
noctave <- 6
nvoice <- 20
dt <- 0.004
i1 <- sort(rep(c(1:noctave), times=nvoice))
  jj <- rep(c(0:(nvoice-1)), times=noctave)

  sa <- 2^(i1+jj/nvoice)

  efs <- scal2freqs(sa, dt)




}

\keyword{misc}
