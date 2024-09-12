\name{winseis24}
\Rdversion{1.1}
\alias{winseis24}
\title{Locator for plotseis24
}
\description{Locator for plotseis24
}
\usage{
winseis24(pjj, pch = 3, col = "red")
}
\arguments{
  \item{pjj}{out put of plotseis24}
  \item{pch}{plotting character when clicking}
  \item{col}{color for plotting when clicking}
  
}
\details{
  After extracting 24 hours and plotting with
  plotseis24, use winseis24 to click on the plot and
  return times for further analysis or zooming.
}
\value{
  list:
  \item{hr}{hours picked}
    \item{yr}{year}
    \item{jd}{julian day}
}

\author{
Jonathan M. Lees<jonathan.lees@unc.edu>
}


\seealso{plotseis24, getseis24
}
\examples{

if(interactive()){
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

######  here is the picking:
wpicks = winseis24(pjj)

}

}
\keyword{misc}
