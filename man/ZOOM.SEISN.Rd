\name{ZOOM.SEISN}
\alias{ZOOM.SEISN}
\title{ZOOM SEISMIC Panel }
\description{
  Zoom interactively on Seismic panel data.
}

\usage{
ZOOM.SEISN(GH, sel = 1:length(GH$dt), WIN = NULL)
}

\arguments{
  \item{GH}{Seismic trace structure }
  \item{sel}{selection of traces }
  \item{WIN}{time window c(0,1) }
}

\value{
   Seismic trace structure
  
}

\author{Jonathan M. Lees<jonathan.lees.edu>}

\seealso{swig}
\examples{


data("GH")
sel <-  which(GH$COMPS=="V")

KF  <-  ZOOM.SEISN(GH, sel=sel, WIN = c(0 , 5)  )

if(interactive()){  swig(KF)
}
}

\keyword{hplot}