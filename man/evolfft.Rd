\name{evolfft}
\alias{evolfft}

\title{Spectrogram fft}
\description{
  Spectrogram using simple fft (Gabor Transform)
}
\usage{
evolfft(a, dt = 0, Nfft = 0, Ns = 0, Nov = 0, fl = 0, fh = 10)
}

\arguments{
  \item{a}{signal}
  \item{dt}{sample rate interval (s) }
  \item{Nfft}{Number of points in fft }
  \item{Ns}{NUmber of sample in sub-window }
  \item{Nov}{number of sample to overlap }
  \item{fl}{ low frequency to display }
  \item{fh}{high frequency to display  }
}
\details{
  This is a duplication of the spectrogram function in matlab  which
  applies Welsh's Method.
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

\author{Jonathan M. Lees<jonathan.lees.edu>}

\seealso{evolMTM, evolAR, MTM.drive}
\examples{


data(CE1)

####  plot signals
plot(CE1$x, CE1$y, type='l')

###  set parameters
Nfft=1024   ###  fft length
Ns=250      ###  number of samples in a window
Nov=240     ###  number of samples of overlap per window
fl=0        ###  low frequency to return
fh=1/(2*CE1$dt)     ###  high frequency to return

########  calculate the evolutive fft (Gabor Transform)
EV = evolfft(CE1$y, dt =CE1$dt , Nfft = Nfft, Ns =Ns , Nov =Nov , fl =fl
, fh = 25)

### plot image, but it does not look too interesting
image(EV$DSPEC)


### plot Gabor transform with special function
PE = plotevol(EV, log=0, fl=0.01, fh=100, col=rainbow(100), ygrid=FALSE,
STAMP="", STYLE="fft")


}

\keyword{misc}
