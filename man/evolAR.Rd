\name{evolAR}
\alias{evolAR}

\title{ Evolutive Auto-Regressive Spectrum }
\description{
   Time varying Auto-Regressive Spectrum (Gabor Transform)
}
\usage{
evolAR(a, dt = 0, numf = 1024, pord = 100, Ns = 0, Nov = 0, fl = 0, fh = 10)
}

\arguments{
  \item{a}{signal}
  \item{dt}{sample rate interval (s)}
  \item{numf}{Number of frequencies}
  \item{pord}{Order for Auto-regressive calculation }
  \item{Ns}{Number of sample in sub-window }
  \item{Nov}{Number of sample to overlap}
  \item{fl}{low frequency to display}
  \item{fh}{high frequency to display}
}
\details{
This is a spectrogram function similar to the Gabor Transform
  but uses the   Auto-Regressive method for spectrum estimation.
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

\author{Jonathan M. Lees<jonathan.lees@unc.edu>}

\seealso{evolfft, evolMTM, MTM.drive, GETARAIC }
\examples{
data(KH)
###   swig(KH)

Xamp <- KH$JSTR[[1]]

dt <- KH$dt[1]
plot(seq(from=0, length=length(Xamp), by=dt), Xamp, type='l')
##  limit the trace, somewhat
Xamp <- Xamp[12670:22669]
plot(seq(from=0, length=length(Xamp), by=dt), Xamp, type='l')


Nfft<-1024   ###  fft length
Ns<-512      ###  number of samples in a window
Nov<-480    ###  number of samples of overlap per window
fl<-0        ###  low frequency to return
fh<-12     ###  high frequency to return

EV <- evolAR(Xamp, dt = dt, numf =Nfft , pord = 100, Ns = Ns,
       Nov = Nov, fl = fl, fh = fh)


PE <- plotevol(EV, log=1, fl=0.01, fh=fh,
      col=rainbow(100), ygrid=FALSE,
      STAMP="", STYLE="ar")


}

\keyword{misc}
