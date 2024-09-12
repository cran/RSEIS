\name{markseis24}
\Rdversion{1.1}
\alias{markseis24}
\title{Mark 24 hour seismic display
}
\description{Mark a 24 hour seismic display
}
\usage{
markseis24(pjj, pix = list(yr = 2009, jd = 1, hr = 0, mi = 0, sec = 0,
dur = 0), col = "red", LEGON = 3, BARON = TRUE, ARROWS = TRUE, lwd=1)
}
\arguments{
  \item{pjj}{Output information from plotseis24 (x,y, yr, jd)
}
\item{pix}{list:
  date list consisting of: yr, jd, hr, mi, sec, dur)
}
  \item{col}{Color, specified as color index, character string or rgb
  }
\item{LEGON}{
      plotting flag for legs: 0=no legs, 1=left leg, 2=right leg, 3=both legs(def
ault)
}
\item{BARON}{
  logical:plotting flag for bar

}
  \item{ARROWS}{logical: plot arrows FALSE=no arrows
  }
  \item{lwd}{numeric, graphical parameter, line width
  }

  
}
\details{
  the LEGON parameter controls the small marks at the ends: Either left(1)
  right(2) both(3) or no legs(0) are plotted.  window bars should wrap
  around the ends of the hour to the next hour below.
  The durations of the windows are supplied in seconds.  If no duration
  is supplied, it is set to 0.  If one duration is supplied it is copied
  to all other windows.
}
\value{Graphical Side effects
}

\author{
Jonathan M. Lees<jonathan.lees@unc.edu>
}


\seealso{winmark, getseis24,  plotseis24
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

###   set up pix
WINS2 <- list(hr = c(12.5, 12.7) )

Apix <- WINS2$hr[seq(from=1, to=length(WINS2$hr), by=2) ]
dur <- (WINS2$hr[seq(from=2, to=length(WINS2$hr), by=2) ]-Apix)*3600

## dur <- rep(0, times=length(Apix))

##  mark the 24 hour plot

pix =list(yr=rep(pjj$yr, length(Apix)),
 jd=rep(pjj$jd, length(Apix)) , hr=Apix, mi=rep(0, length(Apix)),
sec=rep(0, length(Apix)), dur=dur)


markseis24(pjj, pix=pix, col='red', ARROWS=TRUE )



}
\keyword{misc}
