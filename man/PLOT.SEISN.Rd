\name{PLOT.SEISN}
\alias{PLOT.SEISN}

\title{Plot Seismic Section}
\description{
 Seismic traces are plotted on a panel horizontally.
}
\usage{
PLOT.SEISN(GH, tim = 1, dt = 1, sel =c(1:4) , WIN =c(1,0) , labs =c("CE1") , notes = "CE1.V", tags ="CE1.V" , sfact = 1, LOG = "", COL = 'red', add = 1, pts = FALSE, YAX = FALSE, TIT = NULL, SHIFT = NULL, rm.mean = TRUE, UNITS = "volts", MARK = TRUE)
}

\arguments{
  \item{GH}{seismic data structure}
  \item{tim}{tim axis vector, seconds}
  \item{dt}{ deltaT, sample rate }
  \item{sel}{select which traces from GH}
  \item{WIN}{initial time window for plot}
  \item{labs}{character string vector, labels}
  \item{notes}{character string vector, labels}
  \item{tags}{character string vector, labels }
  \item{sfact}{scaling flag}
  \item{LOG}{log for x-axis}
  \item{COL}{col vector}
  \item{add}{add to plot}
  \item{pts}{add points}
  \item{YAX}{ Yaxis label }
  \item{TIT}{title }
  \item{SHIFT}{shift traces }
  \item{rm.mean}{remove mean from traces}
  \item{UNITS}{ character, units of traces }
  \item{MARK}{character marking for earthquake}
}
\details{
 panel of N traces are plotted.
}
\value{
  Graphical Side effect.
  list(n=nn, dy=dy,  minS=minS, maxS=maxS, meanS=meanS, DX=range(tim[tflag]) )
}

\author{Jonathan M. Lees<jonathan.lees.edu>}

\seealso{PICK.GEN }
\examples{

data("GH")

### select only vertical components
sel= which(GH$COMPS=="V")

PLOT.SEISN(GH, sel=sel)


}

\keyword{hplot}
