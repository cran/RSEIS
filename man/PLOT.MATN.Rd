\name{PLOT.MATN}
\alias{PLOT.MATN}

\title{plot a matrix of several seismograms}
\description{
 Matrix of several seismograms
}
\usage{
PLOT.MATN(ascd, tim=1, dt=1, T1=0,  WIN=c(0,1), labs="",
notes=notes, sfact=1,ampboost=0,  shift=NULL, LOG="",
COL='red', add=1, AXES=1, units=NULL, VS=FALSE)
}

\arguments{
  \item{ascd}{N by K matrix of seismograms where }
  \item{tim}{time values fo x-axis}
  \item{dt}{sample interval, seconds}
  \item{T1}{Time for starting sample (default=0)}
  
  \item{WIN}{vector, time window for zoom}
  \item{labs}{vector of labels for each panel}
  \item{notes}{vector of notes for each panel}
  \item{sfact}{scaling factor, 1=window, 2=trace}
   \item{ampboost}{increase each amplitude by this multiplier}
   \item{shift}{vector, shift each trace by these time}
  \item{LOG}{log x-axis}
  \item{COL}{vector of colors or indexes to colors}
  \item{add}{numeric, to existing plot.
    add = 1,2,3  if add=1 plot and add traces,
    add =2 plot, but no traces,
    add = 3 no plot, but add traces.  DEFAULT=1
  }
  \item{AXES}{numeric, 0,1,2,3,4; default=1}
  \item{units}{label for units of Y-axis}
  \item{VS}{ var-squiggle display}
  
}
\details{
 Plots a matrix of seismograms that each have the same starting time.
 For the AXES argument, 0 = no axes,    AXES=1  plot scale for largest
 amplitude band and a multiplier for all others,
 AXES=2 left side, AXES=3 right side, AXES=4 alternate sides
}
\value{
  Graphical side effects and,
 \item{n}{n}
  \item{ windiv}{matrix of n rows, with columns=(window Y min, window Y
    max, user Y min, user Y max) }
}

\author{Jonathan M. Lees<jonathan.lees@unc.edu>}

\seealso{swig, matsquiggle, dowiggles, varsquiggle}
\examples{

dt  <-  0.001

 t  <-  seq(0, 6, by=0.001)

thefreqs  <-  seq(from=10, to=100, by=10)
theamps  <-   runif(length(thefreqs))

#   sample rate is 1000 Hz, 0.001 seconds 601 samples
 x  <-  NULL

for(i in 1:length(thefreqs))
{
x  <-  cbind(x, theamps[i]*sin(2*pi*thefreqs[i]*t))


}

PLOT.MATN(x,  dt = dt)


}

\keyword{hplot}
