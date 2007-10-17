\name{evolfft}
\alias{evolfft}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{spectrogram}
\description{
  spectrogram using Welsh's Method
}
\usage{
evolfft(a, dt = 0, Nfft = 0, Ns = 0, Nov = 0, fl = 0, fh = 10)
}
%- maybe also 'usage' for other objects documented here.
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

\seealso{MTM}
\examples{


data(CE1)
plot(CE1$x, CE1$y, type='l')
Nfft=1024
Ns=250
Nov=240
fl=0
fh=1/(2*CE1$dt)

EV = evolfft(CE1$y, dt =CE1$dt , Nfft = Nfft, Ns =Ns , Nov =Nov , fl =fl , fh
= 25)
image(EV$DSPEC)



}

\keyword{misc}
