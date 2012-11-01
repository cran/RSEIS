\name{doGABOR.MTM}
\Rdversion{1.1}
\alias{doGABOR.MTM}
\title{Evolutive MTM Spectrum
}
\description{ Time varying Auto-Regressive Spectrum (Gabor Transform)
  using MTM.  This is a driver for MTMgabor.
}
\usage{
doGABOR.MTM(Xamp, DT = 0.008, ppoint=95 , multi = 1,
 scale.def = 0, TWIN = 2, TSKIP = 0.2, PCTTAP = 0.05,  PLOT=TRUE)
}
\arguments{
  \item{Xamp}{signal
}
  \item{DT}{sample rate interval (s)
  }
   \item{ppoint}{percent confidence for F-test (default=95) }
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
 This is a spectrogram function similar to the Gabor Transform
 but uses the MTM (multi-taper method) for spectrum estimation.
 This is a non-interactive version of MTM.drive.

  
}
\value{

list output of MTMgabor:
\item{sig}{input signal}
\item{dt}{deltat}
\item{numfreqs}{Number of frequencies output}
\item{wpars}{input parameters list(Nfft=numfreqs,
  Ns=Ns, Nov=Nov, fl=fl, fh=fh)}
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
\references{
 Lees, J. M. and Park, J., 1995: Multiple-taper spectral analysis: A
  stand-alone C-subroutine, \emph{Computers \& Geology}, 21(2), 199-236.

  Percival, Donald B.,Walden, Andrew T. (1993):Spectral Analysis for
  Physical Applications,Cambridge University Press, Cambridge,  583p.
  
  
}
\author{
Jonathan M. Lees<jonathan.lees@unc.edu>
}
\note{
  The main difference between this and other similar calls is the
  way the windows are determined.
}


\seealso{MTMgabor, evolfft, evolMTM, MTM.drive, GETARAIC, doGABOR.AR, DOsgram}
\examples{



data(KH)
###   swig(KH)

Xamp = KH$JSTR[[1]]
Xamp = Xamp[57914:72989]

EV = doGABOR.MTM(Xamp, DT = KH$dt[1], multi = 1, scale.def = 0,
TWIN = 2, TSKIP = 0.2, PCTTAP = 0.05)

}
\keyword{misc}
