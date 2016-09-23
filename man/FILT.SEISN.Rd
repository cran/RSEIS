\name{FILT.SEISN}
\alias{FILT.SEISN}

\title{Filter Traces}
\description{
 Filter Traces in a seismic structure
}
\usage{
FILT.SEISN(TH, sel = 1:length(TH$JSTR),
FILT = list(ON = TRUE, fl = 0.5, fh = 7, type = "HP",
 proto = "BU", RM=FALSE, zp=TRUE ), TAPER = 0.1, POSTTAPER = 0.1)
}

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
Fdef  <-  list(ON=FALSE, fl=0.5, fh=7.0, type="BP", proto="BU", RM=FALSE, zp=TRUE )
data("GH")
sel <-  which(GH$COMPS=="V")

sel  <-  1:3
 KF  <-  FILT.SEISN(GH, sel = sel, FILT=Fdef)
swig(KF, sel=sel, SHOWONLY=0)


}

\keyword{misc}
