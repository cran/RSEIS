\name{pickseis24}
\Rdversion{1.1}
\alias{pickseis24}
\title{Pick zooms on 24 hour display
}
\description{
  Pick zooms on 24 hour display.
}
\usage{
pickseis24(pjj, DB, usta, ucomp)
}
\arguments{
  \item{pjj}{output of plotseis24}
  \item{DB}{Database of seismic trace meta data}
  \item{usta}{stations to extract}
  \item{ucomp}{components to extract}
}
\details{
  Use only 2 clicks on the plotseis24 display.
}
\value{
  Graphical Side effects.  Program starts swig
}

\author{
Jonathan M. Lees<jonathan.lees@unc.edu>
}


\seealso{swig, winseis24 , plotseis24 , getseis24
}
\examples{

\dontrun{

dev.new(width=8, height=12)
dev.new(width=12, height=8)

dev.set(2)
usta = "CAL"
acomp = "V"
JJ = getseis24(DB, 2009, 2, usta, acomp, kind = 1)
pjj = plotseis24(JJ, dy=1/18, FIX=24, SCALE=1, FILT=list(ON=FALSE, fl=0.05 , fh=20.0, type="BP", proto="BU"), RCOLS=c(rgb(0.2, .2, 1), rgb(.2, .2, .2))  )

ucomp = c("V", "N", "E", "I")

pickseis24(pjj, DB, usta, ucomp )


}


}
\keyword{misc}
