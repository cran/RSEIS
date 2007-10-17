\name{jmlcrc}
\alias{jmlcrc}

\title{Ridge Extraction by Crazy Climbers}
\description{
  Uses the "crazy climber algorithm" to detect ridges in the modulus
     of a continuous wavelet or a Gabor transform.

}
\usage{
jmlcrc(tfrep, tfspec = numeric(dim(tfrep)[2]), bstep = 3, iteration = 10000, rate = 0.001, seed = -7, nbclimb = 10, flag.int = TRUE, chain = TRUE, flag.temp = FALSE)
}

\arguments{
  \item{tfrep}{modulus of the (wavelet or Gabor) transform.}
  \item{tfspec}{numeric vector which gives, for each value of the scale or
          frequency the expected size of the noise contribution.}
  \item{bstep}{stepsize for random walk of the climbers.}
  \item{iteration}{number of iterations.}
  \item{rate}{initial value of the temperature.}
  \item{seed}{initial value of the random number generator.}
  \item{nbclimb}{number of crazy climbers.}
  \item{flag.int}{if set to TRUE, the weighted occupation measure is computed.}
  \item{chain}{if set to TRUE, chaining of the ridges is done.}
  \item{flag.temp}{if set to TRUE: constant temperature.}
}

\value{
  Returns a 2D array called beemap containing the (weighted or
     unweighted) occupation measure (integrated with respect to time)

  
}
\references{See discussion in text of ``Practical Time-Frequency Analysis''.}


\seealso{'corona', 'icm', 'coronoid', 'snake', 'snakoid' for ridge }

\keyword{misc}
