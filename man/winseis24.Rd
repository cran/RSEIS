\name{winseis24}
\Rdversion{1.1}
\alias{winseis24}
\title{Locator for plotseis24
}
\description{Locator for plotseis24
}
\usage{
winseis24(pjj, pch = 3, col = "red")
}
\arguments{
  \item{pjj}{out put of plotseis24}
  \item{pch}{plotting character when clicking}
  \item{col}{color for plotting when clicking}
  
}
\details{
  After extracting 24 hours and plotting with
  plotseis24, use winseis24 to click on the plot and
  return times for further analysis or zooming.
}
\value{
  list:
  \item{hr}{hours picked}
    \item{yr}{year}
    \item{jd}{julian day}
}

\author{
Jonathan M. Lees<jonathan.lees@unc.edu>
}


\seealso{plotseis24, getseis24
}
\examples{


\dontrun{
JJ <- getseis24(DB, 2008, 366, usta, acomp, kind = 1)
 
pjj <- plotseis24(JJ, dy=1/18, FIX=24, SCALE=1,
FILT=list(ON=FALSE, fl=0.05 , fh=20.0, type="BP", proto="BU"),
RCOLS=c(rgb(0.2, .2, 1), rgb(.2, .2, .2))  )

winseis24(pjj)


}


}
\keyword{misc}
