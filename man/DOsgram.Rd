\name{DOsgram}
\Rdversion{1.1}
\alias{DOsgram}
\title{Gabor transform
}
\description{Gabor transform with simple spectrum
}
\usage{
DOsgram(Xamp, DT = 0.008, multi = 1, scale.def = 0, TWIN = 2, TSKIP = 0.2, PCTTAP = 0.05, PLOT=TRUE)
}
\arguments{
   \item{Xamp}{signal
}
  \item{DT}{sample rate interval (s)
}
  \item{multi}{Multiples of time window estimate
}
  \item{scale.def}{scaling flag for plotting (0=raw, 1=log, 2=sqrt)
}
  \item{TWIN}{time for window
}
  \item{TSKIP}{time for skip
}
  \item{PCTTAP}{percent of taper to apply to individual windows
  }
   \item{PLOT}{ logical, TRUE=plot to device } 
}
\details{

This is a non-interactive version of SPECT.drive.

  
}
\value{


list

\item{sig}{input signal}
\item{dt}{deltat}
\item{numfreqs}{Number of frequencies output}
\item{wpars}{input parameters list(Nfft=numfreqs,  Ns=Ns, Nov=Nov, fl=fl, fh=fh)}
\item{DSPEC}{spectrum image}

\item{freqs}{output frequencies (y axis)}
\item{tims}{output times (x-axis) }
  
  
}
\references{

   Lees, J. M. and Park, J., 1995: Multiple-taper spectral analysis: A
  stand-alone C-subroutine, \emph{Computers \& Geology}, 21(2), 199-236.

}
\author{
Jonathan M. Lees<jonathan.lees@unc.edu>
}


\seealso{evolMTM, evolfft, evolAR, plotevol
}
\examples{





data(KH)
###   PICK.GEN(KH)

Xamp = KH$JSTR[[1]]
Xamp = Xamp[57914:72989]

Nfft=1024   ###  fft length
Ns=512      ###  number of samples in a window
Nov=480    ###  number of samples of overlap per window
fl=0        ###  low frequency to return
fh=12     ###  high frequency to return


EV = DOsgram(Xamp, DT = 0.008, multi = 1, scale.def = 0, TWIN = 2, TSKIP = 0.2, PCTTAP = 0.05)






}
\keyword{misc}
