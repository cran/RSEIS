\name{DISPLACE.SEISN}
\alias{DISPLACE.SEISN}

\title{Displacement seismogram}
\description{
 Removes seismic instrument response and integrates to displacement.
}
\usage{
DISPLACE.SEISN(TH, sel = 1:length(TH$JSTR), inst = 1,
Kal = Kal,waterlevel = 1e-08,  FILT = list(ON = FALSE,
 fl = 1/30, fh = 7, type = "HP", proto = "BU"))
}

\arguments{
  \item{TH}{list structure of seismic traces}
  \item{sel}{select which tracesin list to deconvolve}
  \item{inst}{index to instrument in Kal list for calibration and
    instrument response }
  \item{Kal}{list of instrument responses}

  \item{waterlevel}{waterlevel for low frequency division }

  \item{FILT}{filter output, after instrumentation}
}
\details{
  Instrument responses are lists of poles and zeros for each instrument defined.
}
\value{
Same as input list with new traces representing displacement versus velocity
}

\author{Jonathan M. Lees<jonathan.lees@unc.edu>}

\seealso{VELOCITY.SEISN, deconinst }
\examples{

\dontrun{
Majorevent   <-  250+8/24+2250/(24*3600)

at1  <-  Majorevent - 1/24
at2  <-  Majorevent + 1/24


TH  <-   Mine.seis(at1, at2, DB99, NULL, NULL , kind = 1,
 Iendian=1, BIGLONG=FALSE, CHOP=TRUE, verbose=FALSE)
inst  <-  rep(0, length(TH$STNS))
inst[TH$COMPS \%in\% c("V", "N", "E")]  <-  1
inst[TH$STNS=="krm2" &  TH$COMPS \%in\% c("V", "N", "E")]  <-  2
inst[TH$STNS=="krm0" &  TH$COMPS \%in\% c("V", "N", "E")]  <-  2


DH  <-   DISPLACE.SEISN(TH, sel = which(inst>0) , inst = inst,
Kal = Kal, FILT = list(ON = FALSE, fl = 1/200, fh = 7,
type = "BP", proto = "BU"))

  SOUT  <-   swig(DH, PADDLAB=c("CENTER", "fspread", "HALF", "PREV"),
filters=thefilts  )


}



}


\keyword{misc}
