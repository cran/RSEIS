\name{DISPLACE.SEISN}
\alias{DISPLACE.SEISN}

\title{Displacement seismogram}
\description{
 Removes seismic instrument response and integrates to displacement.
}
\usage{
DISPLACE.SEISN(TH, sel = 1:length(TH$JSTR), inst = 1, Kal = Kal, FILT = list(ON = FALSE, fl = 1/30, fh = 7, type = "HP", proto = "BU"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{TH}{list structure of seismic traces}
  \item{sel}{select which tracesin list to deconvolve}
  \item{inst}{index to instrument in Kal list for calibration and
    instrument response }
  \item{Kal}{list of instrument responses}
  \item{FILT}{filter output, after instrumentation}
}
\details{
  Instrument responses are lists of poles and zeros for each instrument defined.
}
\value{
Same as input list with new traces representing displacement versus velocity
}

\author{Jonathan M. Lees<jonathan.lees@unc.edu>}

\seealso{VELOCITY.SEISN}

\keyword{misc}
