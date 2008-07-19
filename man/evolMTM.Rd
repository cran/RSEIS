\name{evolMTM}
\alias{evolMTM}

\title{Evolutive Multi-taper Spectrum  }
\description{
  Time varying Multi-taper Spectrum (Gabor Transform)
}
\usage{
evolMTM(a, dt = 0, numf = 1024, Ns = 0, Nov = 0, fl = 0, fh = 10)
}

\arguments{
    \item{a}{Signal}
  \item{dt}{Sample rate interval (s) }
  \item{numf}{Number of points in fft }
  \item{Ns}{Number of sample in sub-window }
  \item{Nov}{Number of sample to overlap }
  \item{fl}{ low frequency to display }
  \item{fh}{high frequency to display  }

}
\details{
  This is a spectrogram function similar to the Gabor Transform
  but uses the MTM method for spectrum estimation.
}
\value{

    List

\item{sig}{input signal}
\item{dt}{deltat}
\item{wpars}{input parameters}
\item{DSPEC}{spectrum image}
\item{freqs}{output frequencies (y axis)}
\item{tims}{output times (x-axis) }

 
}
\references{Lees, J. M. and Park, J., 1995: Multiple-taper spectral
  analysis: A stand-alone C-subroutine,
  {\emph Computers & Geology}, 21(2), 199-236.}

\author{Jonathan M. Lees<jonathan.lees@unc.edu>}

\seealso{evolfft, MTM.drive}
\examples{

data(KH)
###   PICK.GEN(KH)

Xamp = KH$JSTR[[1]]

dt = KH$dt[1]
plot(seq(from=0, length=length(Xamp), by=dt), Xamp, type='l')
##  limit the trace, somewhat
Xamp = Xamp[12670:22669]
plot(seq(from=0, length=length(Xamp), by=dt), Xamp, type='l')


Nfft=4096   ###  fft length
Ns=512      ###  number of samples in a window
Nov=480    ###  number of samples of overlap per window
fl=0        ###  low frequency to return
fh=12     ###  high frequency to return

EV = evolMTM(Xamp, dt = dt, numf = Nfft, Ns = Ns, Nov = Nov, fl = fl, fh
= fh)


PE = plotevol(EV, log=1, fl=0.01, fh=fh, col=rainbow(100), ygrid=FALSE,
STAMP="", STYLE="ar")


##   compare with:
## EVf = evolfft(Xamp, dt = dt, Nfft =Nfft , Ns =Ns , Nov =Nov , fl =fl, fh = fh)

##  PE = plotevol(EVf, log=1, fl=fl, fh=fh, col=rainbow(100), ygrid=FALSE,STAMP="", STYLE="fft")



}

\keyword{misc}
