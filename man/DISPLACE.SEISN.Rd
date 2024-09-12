\name{DISPLACE.SEISN}
\alias{DISPLACE.SEISN}

\title{Displacement seismogram}
\description{
 Removes seismic instrument response and integrates to displacement.
}
\usage{
DISPLACE.SEISN(TH, sel = 1:length(TH$JSTR), inst = 1,
Kal = Kal,waterlevel = 1e-08,  FILT = list(ON = FALSE,
 fl = 1/30, fh = 7, type = "HP", proto = "BU",RM=FALSE, zp=TRUE))
}

\arguments{
  \item{TH}{list structure of seismic traces}
  \item{sel}{select which tracesin list to deconvolve}
  \item{inst}{index to instrument in Kal list for calibration and
    instrument response }
  \item{Kal}{list of instrument responses}

  \item{waterlevel}{waterlevel for low frequency division }

  \item{FILT}{filter output, after instrumentation, see butfilt}
}
\details{
  Instrument responses are lists of poles and zeros for each instrument defined.
}
\value{
Same as input list with new traces representing displacement versus velocity
}

\author{Jonathan M. Lees<jonathan.lees@unc.edu>}

\seealso{VELOCITY.SEISN, deconinst, butfilt }
\examples{

data(KH)

Kal <- PreSet.Instr()

DH  <-   DISPLACE.SEISN(KH, sel = 1 , inst = 1,
Kal = Kal, FILT = list(ON = FALSE, fl = 1/200, fh = 7,
type = "BP", proto = "BU"))

if(interactive()){
  SOUT  <-   swig(DH, PADDLAB=c("CENTER", "fspread", "HALF", "PREV")  )
}



}


\keyword{misc}
