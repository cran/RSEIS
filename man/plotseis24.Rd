\name{plotseis24}
\Rdversion{1.1}
\alias{plotseis24}
\title{Plot 24 hours of seismic data
}
\description{Plot 24 hours of seismic data using output
  of getseis24.
}
\usage{
plotseis24(JJ, dy = 1/18, FIX = 24, SCALE = 0, FILT = list(ON = FALSE,
fl = 0.05, fh = 20, type = "BP", proto = "BU"), RCOLS = c(rgb(0.2, 0.2,
1), rgb(0.2, 0.2, 0.2)), add=FALSE )
}
\arguments{
  \item{JJ}{output list of getseis24
}
  \item{dy}{Delta-y in percentage of trace
}
  \item{FIX}{Fix 24 hour plot.  If FIX is less than 24, the plot will
    show only that number of hours.
}
  \item{SCALE}{scale, 0=scale each trace, 1=scale window
}
  \item{FILT}{filter data
}
  \item{RCOLS}{colors
  }
  \item{add}{logical, if TRUE, add to existing plot (i.e. do not issue a
    plot command)}
  
}
\details{
  Plots full 24 hours of data.
  The list returned can be used by winseis24
  to get picks and windows for zooming.

  The FIX argument is currently not available.
}
\value{
  list:
\item{x}{x-axis}
\item{y}{y-axis}
\item{yr}{year}
\item{jd}{julian day}
        
}

\author{
Jonathan M. Lees<jonathan.lees@unc.edu>
}

\seealso{getseis24, winseis24
}
\examples{


data(KH)

amp = KH$JSTR[[1]]
OLDdt = KH$dt[1]
newdt = 0.1
yr = 2000
GIVE = FAKEDATA(amp, OLDdt=0.01, newdt = 0.1, yr = 2000,
         JD = 4, mi = 12, sec = 0,  Ntraces = 24*3,
seed=200, noise.est=c(1, 100) , verbose=TRUE  )

tdir = tempdir()
for(i in 1:length(GIVE) )
{
    sig = GIVE[[i]]
  d1 =  dateStamp(sig$DATTIM,  sep='_')
   nam1 =  paste(d1,sig$sta, sig$comp, sep='_')
    nam2 = paste0(nam1, '.RDS')
    nam3 = paste(tdir, nam2, sep='/')
    saveRDS(file=nam3, sig) 
    }

########################  Now  read files and make the DataBase:
LF  = list.files(path=tdir, pattern='.RDS', full.names=TRUE)

DB = FmakeDB(LF, kind=-1)

IDB = infoDB(DB)


START =  list(yr =yr , jd= 5 , hr= 0 , mi= 0 ,sec= 0)

END = list(yr =yr , jd= 7 , hr= 0 , mi= 0 ,sec= 0)   


h = getseis24(DB, iyear = 2000, iday = 5, usta = IDB$usta,
                     acomp = IDB$ucomp, kind = -1,  Iendian=1, BIGLONG=FALSE)

  pjj <- plotseis24(h, dy=1/18, FIX=24, SCALE=1,
     FILT=list(ON=FALSE, fl=0.05 , fh=20.0, type="BP", proto="BU"),
     RCOLS=c(rgb(0.2, .2, 1), rgb(.2, .2, .2))  )

 
}
\keyword{misc}
