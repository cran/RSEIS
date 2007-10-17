\name{PLOT.MATN}
\alias{PLOT.MATN}

\title{plot a matrix of several seismograms}
\description{
 Matrix of several seismograms
}
\usage{
PLOT.MATN(ascd, tim = 1, dt = 1, WIN = c(0, 1), labs = "", notes = notes, sfact = 1, LOG = "", COL = 'red', add = 1, AXES = FALSE, units = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ascd}{N by K matrix of seismograms where }
  \item{tim}{time values fo x-axis}
  \item{dt}{sample interval, seconds}
  \item{WIN}{vector, time window for zoom}
  \item{labs}{vector of labels for each panel}
  \item{notes}{vector of notes for each panel}
  \item{sfact}{scaling factor, 1=window, 2=trace}
  \item{LOG}{log x-axis}
  \item{COL}{vecotr of colrs or indeces to colors}
  \item{add}{logical, TRUE=add to existing plot}
  \item{AXES}{logical, TRUE=add axes}
  \item{units}{label for units of Y-axis}
}
\details{
 plots a matrix of seismograms that each have the same starting time
}
\value{
 graphical side effects
}

\author{Jonathan M. Lees<jonathan.lees@unc.edu>}

\seealso{PICK.GEN}
\examples{

dt = 0.001

 t = seq(0, 6, by=0.001)

thefreqs = seq(from=10, to=100, by=10)
theamps =  runif(length(thefreqs))

#   % sample rate = 1000 Hz, 0.001 seconds 601 samples
 x = NULL

for(i in 1:length(thefreqs))
{
x = cbind(x, theamps[i]*sin(2*pi*thefreqs[i]*t))


}

PLOT.MATN(x,  dt = dt)


}

\keyword{hplot}
