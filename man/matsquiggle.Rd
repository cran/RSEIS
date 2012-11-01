\name{matsquiggle}
\alias{matsquiggle}
\title{Matrix Seismic Record}
\description{
  Plot a matrix of time series as a var-squiggle display (filled in half
  traces)
}
\usage{
matsquiggle(XMAT, dt1, dist = NULL, thick = 1,
 FLIP = FALSE, filcol='blue', tracecol="black", add=FALSE, PLOT=TRUE)
}
\arguments{
  \item{XMAT}{matrix of traces}
  \item{dt1}{sample interval, s}
  \item{dist}{distance for each trace in the matrix}
  \item{thick}{thickness for each trace to be plotted}
  \item{FLIP}{logical, FALSE (default) plot horizontal, TRUE=plot
    vertical}

  \item{filcol}{color for shading}
  \item{tracecol}{color for trace}
  \item{add}{add traces to existing plot}
  \item{PLOT}{whether to create a new plotting region}

  
}
\details{ see varsquiggle for more details
}
\value{
  side effects.
}

\author{Jonathan M. Lees<jonathan.lees@unc.edu>}

\seealso{varsquiggle, varsquig}
\examples{


data(GH)
m <- match( GH$STNS,    GH$stafile$name)
LATS <- GH$stafile$lat[m]
LONS <- GH$stafile$lon[m]
dees <- rdistaz( GH$pickfile$LOC$lat, GH$pickfile$LOC$lon, LATS, LONS)

sel <- which(GH$COMPS=="V")
sel <- sel[order(dees$dist[sel])]

###  plot normal way:
### swig(GH, sel=sel, WIN=c(5,10), SHOWONLY=TRUE)


###  plot with varsquiggle
### varsquiggle(GH, sel=sel, WIN=c(5,10))

ex <- seq(from=0, by=GH$dt[sel[1]], length=length(GH$JSTR[[sel[1]]]))
wx <- ex>=5 & ex<=10
XMAT <- matrix(ncol=length(sel), nrow=length(which(wx)))

for(i in 1:length(sel))
{
XMAT[,i] <-  GH$JSTR[[sel[i]]][wx]

}


matsquiggle(XMAT, GH$dt[sel[1]] , dist = dees$dist[sel] , thick = 1,
FLIP = FALSE)

axis(1)
axis(2)
title(xlab="Time, s", ylab="Distance, km")


}
\keyword{hplot}
