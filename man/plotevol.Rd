\name{plotevol}
\alias{plotevol}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Plot Spectrogram}
\description{
 Plot Spectrogram
}
\usage{
plotevol(DEVOL, log = 0, fl = 0, fh = 10, col = col, ylog = FALSE, ygrid = FALSE, AXE = c(1, 2, 3, 4), CSCALE = FALSE, STAMP = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{DEVOL}{spectrogram structure }
  \item{log}{scale by logarithm}
  \item{fl}{low frequency }
  \item{fh}{high frequency }
  \item{col}{color palette }
  \item{ylog}{scale Y-axis by log}
  \item{ygrid}{logical, TRUE=add grid}
  \item{AXE}{sides to add axis}
  \item{CSCALE}{logical}
  \item{STAMP}{character string for identification }
}
\details{
 Plot Spectrogram
}
\value{
 Graphical Side Effects
}

\author{Jonathan M. Lees<jonathan.lees.edu>}

\seealso{evolfft}
\examples{


data(CE1)

Xamp = CE1$y

DT = CE1$dt

 tsecs = DT*(length(Xamp)*.02)
    multi = 2
scale.def = 1
  TWOSEC = tsecs*(1/DT)
    
  NS = floor(multi*TWOSEC)
  NOV = floor(multi*(TWOSEC-.2*TWOSEC))

    
    Nfft=4096

    pal = rainbow(100)

    fl  = 0
   fh=1/(2*DT)

   flshow =  .5
   fhshow = 120

   DEV = evolfft(Xamp,DT , Nfft=Nfft, Ns=NS , Nov=NOV,  fl=fl, fh=fh  )

  PE =  plotevol(DEV, log=scale.def, fl=flshow, fh=fhshow,
                 col=pal, ygrid=FALSE, STAMP="HITHERE")




}

\keyword{hplot}
