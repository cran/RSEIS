\name{MTMgabor}
\Rdversion{1.1}
\alias{MTMgabor}
\title{Evolutive MTM Spectrum
}
\description{
  Time varying Auto-Regressive Spectrum (Gabor Transform) using MTM
}
\usage{
MTMgabor(a, dt = 0, ppoint=95 , numf = 1024, Ns = 0, Nov = 0, fl = 0, fh = 10)
}
\arguments{
  \item{a}{signal}
  \item{dt}{sample rate interval (s)}
  \item{ppoint}{percent confidence for F-test (default=95) }
  
  \item{numf}{Number of frequencies}
  \item{Ns}{Number of sample in sub-window }
  \item{Nov}{Number of sample to overlap}
  \item{fl}{low frequency to display}
  \item{fh}{high frequency to display}


}
\details{
  This is a spectrogram function similar to the Gabor Transform
  but uses the MTM (multi-taper method) for spectrum estimation.
   This is a non-interactive version of MTM.drive.

  
}
\value{


List

\item{sig}{input signal}
\item{dt}{deltat}
\item{numfreqs}{Number of frequencies output}
\item{wpars}{input parameters list(Nfft=numfreqs,  Ns=Ns, Nov=Nov, fl=fl, fh=fh)}
\item{DSPEC}{spectrum image}
\item{HIMAT}{matrix with high values of F-test at 90 percent confidence
}
\item{DOFMAT}{Matrix image of degrees of freedom}
\item{FVMAT}{Matrix image of F-test values}
\item{kdof}{test degrees of freedom=2*nwin-2}

\item{ppoint}{percentage point for confidence bounds}

\item{freqs}{output frequencies (y axis)}
\item{tims}{output times (x-axis) }

}
\references{Percival and Walden;

  Lees, J. M. and Park, J., 1995: Multiple-taper spectral analysis: A
  stand-alone C-subroutine, \emph{Computers and Geology}, 21(2), 199-236.

  Percival, Donald B.,Walden, Andrew T. (1993):Spectral Analysis for
  Physical Applications,Cambridge University Press, Cambridge,  583p.
  
  
  
}
\author{
Jonathan M. Lees<jonathan.lees@unc.edu>
}

\seealso{evolfft, evolMTM, MTM.drive, GETARAIC, doGABOR.AR, DOsgram, doGABOR.MTM
}
\examples{


\dontrun{
data(KH)
###   swig(KH)

Xamp <- KH$JSTR[[1]]
Nfft <- 1024   ###  fft length
Ns <- 512      ###  number of samples in a window
Nov <- 480    ###  number of samples of overlap per window
fl <- 0        ###  low frequency to return
fh <- 12     ###  high frequency to return
 dt  <-  KH$dt[1]


EV <-   MTMgabor(Xamp, dt = dt, numf =Nfft , Ns = Ns, Nov = Nov, fl = fl, fh= fh)

PE   <- plotevol(EV, log=1, fl=0.01, fh=fh, col=rainbow(100),
              ygrid=FALSE, STAMP="", STYLE="ar")

}


}
\keyword{misc}
