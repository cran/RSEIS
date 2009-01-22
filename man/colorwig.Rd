\name{colorwig}
\alias{colorwig}
\title{Plot a seimic trace colored in time}
\description{
  Plot a seimic trace colored in time.  useful for coordinating other
  plots to specific times along a seismic trace.
}
\usage{
colorwig(x1, y1, COL = rainbow(100))
}
\arguments{
  \item{x1}{x-coordinate (time)}
  \item{y1}{y-coordinate (amplitude)}
  \item{COL}{color palette}
}

\value{
Graphical Side Effects
}

\author{Jonathan M. Lees<jonathan.lees@unc.edu>}


\examples{

data(KH)

x = KH$ex[KH$ex>95& KH$ex<125]
y = KH$JSTR[[1]][KH$ex>95& KH$ex<125]


colorwig(x , y , rainbow(100))




}
\keyword{misc}
