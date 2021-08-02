\name{TAPER.SEISN}
\alias{TAPER.SEISN}

\title{Taper Traces}
\description{
 Taper  traces in a seismic structure using a cosine function on the ends.
}
\usage{
TAPER.SEISN(TH, sel = 1:length(TH$JSTR),  TAPER = 0.1 )
}

\arguments{
  \item{TH}{Seismic structure}
  \item{sel}{selection of traces }

  \item{TAPER}{filter taper, percent cosine taper}
  
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


data("GH")
sel <-  which(GH$COMPS=="V")

sel  <-  1:3
 KF  <-  TAPER.SEISN(GH, sel = sel, TAPER=0.1)
swig(KF, sel=sel, SHOWONLY=0)


}

\keyword{misc}
