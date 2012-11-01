\name{pwlet2freqs}
\alias{pwlet2freqs}

\title{Convert Wavelet Axis to Frequency}
\description{
  Convert Wavelet Axis to Frequency
}
\usage{
pwlet2freqs(noctave, nvoice, dt, flip = TRUE,
tab.FREQ, plot = FALSE, perc = 0.85)
}

\arguments{
  \item{noctave}{number of octives}
  \item{nvoice}{number of voices }
   \item{dt}{sample rate (s)}
  \item{flip}{logical,  whether to flip the orientation }
  \item{tab.FREQ}{vector of frequencies}
  \item{plot}{logical, TRUE=add to plot}
  \item{perc}{percent of range to consider}
}
\details{
This function is used to add a y-axis to a wavelet transform plot.
}
\value{
  list:
  \item{why}{y-axis coordinate on wavelet transform}
  \item{Iat}{location}
  \item{efs}{frequencies}
}

\author{Jonathan M. Lees<jonathan.lees.edu>}

\seealso{wlet.do}
\examples{
pfreqs <- c(0.5, 1, 2,3,4,5, 10, 14)

zp <- pwlet2freqs(noctave= 6, nvoice= 20, 0.004,
 flip = TRUE, pfreqs, plot = FALSE, perc = 0.85)
  
}

\keyword{misc}
\keyword{aplot}