\name{FAKEDATA}
\alias{FAKEDATA}
\title{Fake Data for Examples.
}
\description{
  Create a list of artifical seismic traces to illustrate
  examples that require a database or long sequences.
}
\usage{
FAKEDATA(amp, OLDdt = 0.01, newdt = 0.1, yr = 2000,
JD = 5, mi = 0, sec = 0, Ntraces = 48, seed = 200,
noise.est = c(1, 100), verbose = FALSE)
}
\arguments{
  \item{amp}{vector, some signal that will be repeated
}
  \item{OLDdt}{Orignal sample rate
}
  \item{newdt}{New sample rate, usually less than the original
}
  \item{yr}{year
}
  \item{JD}{starting Julian day
}
  \item{mi}{starting minute
}
  \item{sec}{starting second
}
  \item{Ntraces}{number of traces
}
  \item{seed}{random seed
}
  \item{noise.est}{2-vector, starting and ending sample to estimate
    noise level of trace
}
  \item{verbose}{logical, message feed back
}
}
\details{
  The input signal can be any time series, or even a made up signal.
  This is just to give the look of the result something like real data.
  The noise level is extracted from the man and std of the real data
  at the samples indicated by noise.est.

  The sampling rate (dt, sec/sample ) is increased mainly for speed and plotting.  This
  may  be skipped for certain functions involving spectrum analysis.

  The signal is distributed randomly in each hour along the total span
  of the requested period, i.e. each hour has one instance of the signal.
  
  The date is arbitrary, of course.
  
  
}
\value{List of data in a format similar to the output of GET.seis.
}

\author{
Jonathan M. Lees<jonathan.lees@unc.edu>
}

\seealso{GET.seis
}
\examples{


#####  get a time series
data(KH)

amp = KH$JSTR[[1]]
OLDdt = KH$dt[1]
####  downsample to:
newdt = 0.1

JK = FAKEDATA(amp, OLDdt=OLDdt, newdt = 0.1, yr = 2000,
         JD = 4, mi = 12, sec = 0,  Ntraces = 3,
seed=200, noise.est=c(1, 100) , verbose=TRUE  )

 op <- par(no.readonly = TRUE)
par(mfrow=c(length(JK), 1) )
for(i in 1:length(JK) )
{
    DATTIM = paste(c(unlist(JK[[i]]$DATTIM), JK[[i]]$N), collapse=' ')
    
    plotGH( JK[[i]] )
    mtext(DATTIM, side=3, at=JK[[i]]$DATTIM$t2/2)
    
}
 par(op)


}
\keyword{misc}
