\name{PLOT.SEISN}
\alias{PLOT.SEISN}

\title{Plot Seismic Section}
\description{
 Seismic traces are plotted on a panel horizontally.
}
\usage{
PLOT.SEISN(GH, tim = 1, dt = 1, sel =c(1:4) , WIN =c(1,0) , labs =c("CE1") , notes = "CE1.V", tags ="CE1.V" , sfact = 1, LOG = "", COL = 'red', add = 1, pts = FALSE, YAX = 1, TIT = NULL, SHIFT = NULL, rm.mean = TRUE, UNITS = "volts", MARK = TRUE)
}

\arguments{
  \item{GH}{seismic data structure}
  \item{tim}{tim axis vector, seconds}
  \item{dt}{ deltaT, sample rate }
  \item{sel}{select which traces from GH}
  \item{WIN}{initial time window for plot}
  \item{labs}{character string vector, labels}
  \item{notes}{character string vector, labels}
  \item{tags}{character string vector, labels }
  \item{sfact}{scaling flag}
  \item{LOG}{log for x-axis}
  \item{COL}{col vector}
  \item{add}{add to plot}
  \item{pts}{add points}
  \item{YAX}{ type of Yaxis label, 1,2,3 DEFAULT=1 }
  \item{TIT}{title }
  \item{SHIFT}{shift traces }
  \item{rm.mean}{remove mean from traces}
  \item{UNITS}{ character, units of traces }
  \item{MARK}{character marking for earthquake}
}
\details{
 panel of N traces are plotted.  For YAX, default is YAX=1, plot an axis
 with no units label and scale all the traces to 
}
\value{
  Graphical Side effect.
  list(n=nn, dy=dy,  minS=minS, maxS=maxS, meanS=meanS, DX=range(tim[tflag]) )
}

\author{Jonathan M. Lees<jonathan.lees.edu>}

\seealso{PICK.GEN }
\examples{

data("GH")
m = match( GH$STNS,    GH$stafile$name)
LATS = GH$stafile$lat[m]
LONS = GH$stafile$lon[m]
dees = distaz( GH$pickfile$LOC$lat, GH$pickfile$LOC$lon, LATS, LONS)

sel = which(GH$COMPS=="V")
sel = sel[order(dees$dist[sel])]


###   set up good colors
pcols = seiscols(GH)

### select only vertical components

PLOT.SEISN(GH, sel=sel)

GH$units = rep("m/s", times=length(GH$KNOTES))
GH$pcols = pcols


######  simple plot of GH structure
YN = PLOT.SEISN(GH, WIN=c(5,12))

######  a color must be provided for all traces.
######  simple plot of GH structure, with selection and colors

YN = PLOT.SEISN(GH, WIN=c(5,12), sel=sel, COL=rainbow(length(sel)) )


####   alternating Y axes
 YN = PLOT.SEISN(GH, WIN=c(5,12) ,  dt=GH$dt[sel], sel=sel, sfact=1 ,
notes=GH$KNOTES[sel], YAX =3, UNITS = TRUE ,labs = GH$units[sel],
COL=pcols    , TIT="test")


#### Y  axes on same side
 YN = PLOT.SEISN(GH, WIN=c(5,12) ,  dt=GH$dt[sel], sel=sel, sfact=1 ,
notes=GH$KNOTES[sel], YAX =2, UNITS = TRUE ,labs = GH$units[sel],
COL=pcols    , TIT="test")








}

\keyword{hplot}
