\name{DISTxsec}
\alias{DISTxsec}
\title{Distance Cross section
}
\description{Plot time series vertically at specified distances.
  Produces a seismic cross section with correct spacing between traces.
}
\usage{
DISTxsec(GH, dist, TIM.WIN = c(0, 3600), sel, trace.width = 10,
col = "black", text.col = "blue", text.font = 2, text.size = 0.8,
 add = FALSE, plot = TRUE)
}
\arguments{
  \item{GH}{RSEIS seismic trace structure, output of prepSEIS used in swig
}
  \item{dist}{distance for each station along x-axis
}
  \item{TIM.WIN}{time window for cross section
}
  \item{sel}{numeric, index of selected traces to plot.  
}
  \item{trace.width}{Width of each trace in plot.  Should be in same
    units as x-axis
}
  \item{col}{color for traces.  If vector, each trace is plotted with
    assigned color.
}
  \item{text.col}{color for text identifying each trace.
}
  \item{text.font}{font  for text identifying each trace.
}
  \item{text.size}{size of text  for identifying each trace.
}
  \item{add}{logical, Whether to add traces, or just set up the figure
}
  \item{plot}{logical, whether to plotthe traces.
}
}
\details{Distances should be a vector for each trace in the RSEIS list.
  
}
\value{vector of x-y coordinates of the plot.
}

\author{
Jonathan M. Lees<jonathan.lees@unc.edu>
}


\seealso{swig, prepSEIS
}
\examples{

#### example using data in the RSEIS package
data(GH)
####  get the source location
lat.org  = GH$pickfile$LOC$lat
lon.org  = GH$pickfile$LOC$lon
####  get the station locations
g1 =GH$stafile
#### find the distance to each station
gd = rdistaz(lat.org, lon.org, g1$lat, g1$lon )

##### optional, filter the data
sel= which( GH$COMPS == 'V')
### filter traces
Fdef  <-  list(ON=TRUE, fl=1, fh=1, type="HP", proto="BU", RM=TRUE, zp=TRUE )
KF  <-  FILT.SEISN(GH, FILT=Fdef)

### match the stations in GH to the station distances
m1 = match(GH$STNS  , g1$name) 
dist.GH = gd$dist[m1]
TIM.WIN = range(GH$ex) 


#######  prepare plot, but do not add traces
A = DISTxsec(KF, dist.GH, TIM.WIN, sel, trace.width = 0.5 , add=FALSE,
plot=FALSE )
#####  add traces
B = DISTxsec(KF, dist.GH, TIM.WIN, sel, trace.width = 0.5 , add=TRUE,
plot=TRUE, col='black'  , text.col='red', text.size=1  )




}
\keyword{misc}
