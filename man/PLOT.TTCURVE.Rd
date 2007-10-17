\name{PLOT.TTCURVE}
\alias{PLOT.TTCURVE}
\title{Plot Seismic Section, travel time curve }
\description{
  Seismic traces are plotted on a panel horizontally, with spacing
  according to distance from source.

}
\usage{
PLOT.TTCURVE(GH, STAXY = NULL, DIST = c(0, 10), DY = 0.1, tim = 1, dt = 1, sel = c(1:4), WIN = c(1, 0), labs = c("CE1"), notes = "CE1.V", tags = "CE1.V", sfact = 1, COL = "red", add = 1, pts = FALSE, YAX = FALSE, TIT = NULL, SHIFT = NULL, rm.mean = TRUE, UNITS = "volts", MARK = TRUE)
}
\arguments{
  \item{GH}{Seismic data Structure}
  \item{STAXY}{Station Locations and distances in KM }
  \item{DIST}{Distance range, km}
  \item{DY}{height of each wiggle}
  \item{tim}{time span for plotting}
  \item{dt}{sample interval, seconds }
  \item{sel}{select which traces to plot}
  \item{WIN}{vector, time window for zoom }
  \item{labs}{vector of labels for each panel }
  \item{notes}{vector of notes for each panel }
  \item{tags}{character string vector, labels }
  \item{sfact}{scaling flag }
  \item{COL}{col vector }
  \item{add}{add to plot}
  \item{pts}{add points}
  \item{YAX}{Yaxis label }
  \item{TIT}{title }
  \item{SHIFT}{shift traces }
  \item{rm.mean}{remove mean from traces }
  \item{UNITS}{character, units of traces }
  \item{MARK}{character marking for earthquake }
}
\details{
  
}
\value{
 Graphical Side effect.

  list(n=nn, dy=dy,  minS=minS, maxS=maxS, meanS=meanS, DX=range(tim[tflag]), DY=DY, DIST=DIST )
 
}
\references{}
\author{Jonathan M. Lees<jonathan.lees@unc.edu>}
\note{
  This program is similar to PLOT.SEISN
  but traces are plotting with increasing distance from a set point.
  The distances are calculated prior to execution and passed as a vector
  or structure.
}
\seealso{PLOT.SEISN}

\keyword{misc}
