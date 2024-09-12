\name{pickseis24}
\Rdversion{1.1}
\alias{pickseis24}
\title{Pick zooms on 24 hour display
}
\description{
  Pick zooms on 24 hour display.
}
\usage{
pickseis24(w, DB, usta, ucomp, kind=-1,  Iendian=1,
     BIGLONG=FALSE)
}
\arguments{
  \item{w}{picking windows from output of plotseis24 and winseis24}
  \item{DB}{Database of seismic trace meta data}
  \item{usta}{stations to extract}
  \item{ucomp}{components to extract}
  \item{kind}{an integer -1, 0, 1, 2 ; 0="RDATA" , -1="RDS", 0="RDATA", 1 = "segy",
    2 = "sac", see notes below}
  \item{Iendian}{ vector,   Endian-ness of the data: 1,2,3: "little", "big", "swap".
    Default = 1 (little)
  }
  \item{BIGLONG}{ logical, TRUE=long=8 bytes
  }
   
  
}
\details{
  Use sequence of 2 clicks per zoom window  on the plotseis24 display.
}
\value{
  Graphical Side effects.  Program starts swig
}

\author{
Jonathan M. Lees<jonathan.lees@unc.edu>
}


\seealso{swig, winseis24 , plotseis24 , getseis24
}
\examples{


if(interactive())
{
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

 w = winseis24(pjj)

dev.new()

pickseis24(w, DB, IDB$usta[1], IDB$ucomp[1] )

}

}
\keyword{misc}
