\name{winmark}
\alias{winmark}

\title{Window Mark}
\description{
 Add Mark up to current seismic trace with a bar desgnating a window
 selection.  
}
\usage{
winmark(a1, a2, side = 1, bar = NULL, leg = NULL, col = col, lwd = 1, lty = 1, arrows = FALSE, alen = 0.1, leglen = 0.15, LEGON = 3, BARON = TRUE)
}

\arguments{
  \item{a1}{x1-location}
  \item{a2}{x2-location }
  \item{side}{side where bar is drawn, as in axes: 1=bottom,2=left,3=top,4=right  }
  \item{bar}{location of bar}
  \item{leg}{location of leg}
  \item{col}{color}
  \item{lwd}{line width}
  \item{lty}{line type}
  \item{arrows}{logical, add arrows to ends of legs}
  \item{alen}{length of arrow heads, inches, default=0.125}
  \item{leglen}{length of arrows aas percent of usr("par"), default=0.125 }
    \item{LEGON}{
      plotting flag for legs: 0=no legs, 1=left leg, 2=right leg, 3=both legs(default)
}
\item{BARON}{
  logical:plotting flag for bar

}
}
\details{
   Used for marking seismic
  traces.  The window marker looks like a staple, three segments are drawn, a bar
  and two legs. The thickness of the legs are determined by
  bar and leg, unless these are missing.
  if they are missing parameter side is used to set the locations, and
  leglen determines the length of the legs.
  If either bar or leg are missing the
  parameters are derived from par("usr") and are applied to whole
  window.
  side switches the orientation
  of the staple mark, with the legs pointing according away from named the
  axis.
}
\value{
 Graphical Side Effect
}

\author{Jonathan M. Lees<jonathan.lees.edu>}
\examples{

plot(c(0,1), c(0,1), type='n', xlab='', ylab='' )


winmark(.3, .7,       side=3, col='brown', arrows=TRUE, leglen=.4)
winmark(.3, .7,       side=1, col='blue', arrows=TRUE, leglen=.5)

winmark(.3, .7,        side=2, col='green', arrows=TRUE, alen=.05, leglen=.4)


winmark(.3, .7,      leg=.65, bar=.6,     side=4, col='orange', arrows=TRUE, alen=.1, leglen=.125)


winmark(.3, .7,      bar=.65, leg=.6,     side=4, col='seagreen', arrows=TRUE, alen=.1, leglen=.125)
#############  examples with different legs showing
plot(c(0,1), c(0,1), type='n', xlab='', ylab='' )

winmark(.3, .7,       side=3, col='brown', arrows=TRUE, leglen=.4, LEGON=1)
winmark(.3, .4,       side=1, col='brown', arrows=TRUE, leglen=.4, LEGON=2)
winmark(.7, .9,       side=1, col='blue', arrows=TRUE, leglen=.4, LEGON=0)


}

\keyword{aplot}
