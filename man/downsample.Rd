\name{downsample}
\alias{downsample}

\title{Interpolate time series at higher sample rate.
}
\description{Interpolate a times series with a higher/lower  sample
  rate for processes that are sensitive to low samples.
}
\usage{

downsample(sig, dt=0.001, newdt=0.01, PLOT=FALSE )
}
\arguments{
  \item{sig}{time series vector
}
  \item{dt}{sample rate s/sample
}
  \item{newdt}{New, lower sample rate
  }
   \item{PLOT}{logical, plot both traces, default=FALSE
  }
}
\details{Linear interpolation is performed between samples.
  If the newdt is an integer multiple of the old dt,
  The samples will not be modified.
}
\value{time series vector with new sample rate.
}

\author{
Jonathan M. Lees<jonathan.lees@unc.edu>
}

\examples{
data(KH)
sig = KH$JSTR[[1]]
####  reduce samples from 125 (0.008)  to 25Hz (0.04) 
newdt = KH$dt[1]*5
sig2 = downsample(sig, dt = KH$dt[1], newdt = newdt )

L0 = length(sig)
L1 = length(sig2)

op <- par(no.readonly = TRUE)
par(mfrow=c(2,1) )
  plot.ts(ts(sig, deltat=KH$dt[1] ), xlab='s',
ylab='Amplitude', main=paste('Orignal', L0) )
grid()
     plot.ts(ts(sig2, deltat=newdt ), xlab='s',
ylab='Amplitude', main=paste('Downsample', L1) )
grid()
par(op)

}
\keyword{misc}
