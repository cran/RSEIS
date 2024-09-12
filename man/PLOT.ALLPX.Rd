\name{PLOT.ALLPX}
\alias{PLOT.ALLPX}

\title{plot all phase arrival picks}
\description{
 plot all phase arrival picks
}
\usage{
PLOT.ALLPX(t0, STNS, COMPS, YPX, PHASE = NULL, POLS = TRUE,
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


data(GH)

WPX = data.frame(GH$pickfile$STAS)
T0 = data.frame(GH$info)[1,]

sel = which(GH$COMPS=='V')
 PLOT.SEISN(GH, sel=sel)

PLOT.ALLPX(T0, GH$STNS, GH$COMPS, WPX, PHASE='P',FORCE=TRUE)




}

\keyword{aplot}
