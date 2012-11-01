\name{swig.ALLPX}
\alias{swig.ALLPX}

\title{plot all phase arrival picks}
\description{
 plot all phase arrival picks
}
\usage{
swig.ALLPX(t0, STNS, COMPS, YPX, PHASE = NULL, POLS = TRUE,
 FILL = FALSE, FORCE = TRUE, cex = cex, srt = srt)
}

\arguments{
  \item{t0}{time for start of window, s}
  \item{STNS}{station names to plot}
  \item{COMPS}{components to plot }
  \item{YPX}{y-picks (times) }
  \item{PHASE}{Phases to plot}
  \item{POLS}{polaritiy information (up, down) }
  \item{FILL}{fill color}
  \item{FORCE}{logical, force all phases plotted on all traces }
  \item{cex}{character expansion}
  \item{srt}{string rotation angle, degrees}
}
\details{
 for use in conjunction with PLOT.SEISN program
}
\value{
 Graphical Side Effect
}

\author{Jonathan M. Lees<jonathan.lees.edu>}

\seealso{PLOT.SEISN, swig}
\examples{


\dontrun{
swig.ALLPX(Torigin, STNS, COMPS, WPX, PHASE=PHASE,
FORCE=forcepix) }

}

\keyword{aplot}
