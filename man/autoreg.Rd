\name{autoreg}
\alias{autoreg}

\title{Auto-Regressive Spectrum Estimate}
\description{
  Auto-Regressive Spectrum Estimate
  }
  
\usage{
autoreg(a, numf = 1024, pord = 100, PLOT = FALSE, f1 = 0.01, f2 = 50)
}

\arguments{
  \item{a}{signal }
  \item{numf}{number of frequency points to calculate }
  \item{pord}{order }
  \item{PLOT}{logical, TRUE=plot }
  \item{f1}{low frequency }
  \item{f2}{high frequency }
}

\value{
  LIST:
  \item{amp}{amplitudes}
  \item{freq}{frequencies, Hz}
 
}

\author{Jonathan M. Lees<jonathan.lees.edu>}

\seealso{fft, mtapspec, plt.MTM0 }
\examples{
data(CE1)
Xamp <- CE1$y[CE1$x>5.443754 & CE1$x<5.615951]

ZIM <- autoreg(Xamp , numf=length(Xamp) , pord = 100, PLOT=FALSE,  f1=.01, f2=50)

}

\keyword{misc}
\keyword{hplot}
