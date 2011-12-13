\name{setWPX}
\alias{setWPX}
\title{Set WPX
}
\description{
Create a WPX list from vector input
or relavent parameters.
}
\usage{
setWPX(phase = NULL, col = NULL, yr = NULL, jd = NULL, hr = NULL, mi = NULL, sec = NULL, dur = NULL, name = NULL, comp = NULL, dispcomp = NULL, onoff = NULL)
}
\arguments{
  \item{phase}{character, phase names
}
  \item{col}{character, colors
}
  \item{yr}{numeric, year
}
  \item{jd}{numeric, julian day
}
  \item{hr}{numeric, hour
}
  \item{mi}{numeric, minute
}
  \item{sec}{numeric, second
}
  \item{dur}{numeric, duration(s)
}
  \item{name}{character, station name
}
  \item{comp}{character, component
}
  \item{dispcomp}{character, display string
}
  \item{onoff}{numeric, flag for turning pick on or off
}
}
\details{
Utility for setting up a WPX list for further
processing.
}
\value{
WPX list.
}

\author{
Jonathan M. Lees<jonathan.lees@unc.edu>
}
\note{Used internally.
}

\seealso{
addWPX,	catWPX,	checkWPX,	cleanWPX,	clusterWPX,	repairWPX,	saveWPX
}
\examples{

s1 = setWPX(name="HI", yr=2011, jd=231, hr=4, mi=3, sec = runif(5)) 


}
\keyword{misc}
