\name{FILT.SEISN}
\alias{FILT.SEISN}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Filter Traces}
\description{
 Filter Traces in a seismic structure
}
\usage{
FILT.SEISN(TH, sel = 1:length(TH$JSTR), FILT = list(ON = TRUE, fl = 0.5, fh = 7, type = "HP", proto = "BU"), TAPER = 0.1, POSTTAPER = 0.1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{TH}{Seismic structure}
  \item{sel}{selection of traces }
  \item{FILT}{filter definition }
  \item{TAPER}{filter taper}
  \item{POSTTAPER}{taper after filter}
}
\details{
  Seismic structure
}
\value{
 Seismic structure
}

\author{Jonathan M. Lees<jonathan.lees.edu>}

\seealso{butfilt}
\examples{

##   Fdef = choosfilt()
Fdef = list(ON=FALSE, fl=0.5, fh=7.0, type="BP", proto="BU")
data("GH")
sel= which(GH$COMPS=="V")

sel = 1:3
 KF = FILT.SEISN(GH, sel = sel, FILT=Fdef)
PICK.GEN(KF)
}

\keyword{misc}
