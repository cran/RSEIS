\name{varsquiggle}
\alias{varsquiggle}
\title{Var-Squiggle Plot}
\description{
  Plot A seismic section using Var-Squiggle, like an exploration
  seismic record.
}
\usage{
varsquiggle(GH, sel = c(1, 2), WIN = c(0, 1), dist=NULL, thick=1 ,
FLIP=FALSE, filcol='blue', tracecol='blue', xpd=TRUE, plotdir=1 )
}

\arguments{
  \item{GH}{Seismic List}
  \item{sel}{selection of seismic traces}
  \item{WIN}{time window}
  \item{dist}{distance from the source}
  \item{thick}{thickness of plotting region per trace}
  \item{FLIP}{logical, whether to plot vertical or horizontal, default
    FALSE,  TRUE = vertical}
   \item{filcol}{color for shading}
  \item{tracecol}{color for trace}
   \item{xpd}{logical, set xpd parameter (see par) }	
   \item{plotdir}{1=left to right, 0=right to left (default=1) }
  
}
\details{
 Traces are plotted and scaled each with its own window. The distance
 vector provides the location on the seismic record.  
}
\value{
Graphical Side effects
}

\author{Jonathan M. Lees<jonathan.lees@unc.edu>}

\seealso{matsquiggle, varsquig}
\examples{
data(GH)
m <- match( GH$STNS,    GH$stafile$name)
LATS <- GH$stafile$lat[m]
LONS <- GH$stafile$lon[m]
dees <- rdistaz( GH$pickfile$LOC$lat, GH$pickfile$LOC$lon, LATS, LONS)

sel <- which(GH$COMPS=="V")
sel <-  sel[order(dees$dist[sel])]

###  plot normal way:
swig(GH, sel=sel, WIN=c(5,10), SHOWONLY=TRUE)


###  plot with varsquiggle
varsquiggle(GH, sel=sel, WIN=c(5,10))


}

\keyword{hplot}
