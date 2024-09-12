\name{getseis24}
\Rdversion{1.1}
\alias{getseis24}
\title{Get 24 Hours of Seismic Data
}
\description{Get 24 Hours of Seismic Data
}
\usage{
getseis24(DB, iyear = 2009, iday = 1, usta = "",
 acomp = "", kind = 1,  Iendian=1, BIGLONG=FALSE)
}
\arguments{
  \item{DB}{
    Data base of meta-data about the seismic trace files
}
  \item{iyear}{Year for extraction
}
  \item{iday}{Julian day for extraction
}
  \item{usta}{station to show
}
  \item{acomp}{component to show
}
  \item{kind}{kind of data, default=1, 0="RDATA" , -1="RDS", 0="RDATA", 1 = "segy",
    2 = "sac"
  }
 \item{Iendian}{ Endian-ness of the data: 1,2,3: "little", "big", "swap".
    Default = 1 (little)
  }
  \item{BIGLONG}{ logical, TRUE=long=8 bytes
  }
  
  
}
\details{
  The DB file consists of a list of information on
  where to find the data and what times are covered.
   DB is \describe{
      \item{fn}{full path to file}
      \item{yr}{year}
      \item{jd}{julian day}
      \item{hr}{hour}
      \item{mi}{minute}
      \item{sec}{second}
      \item{dur}{duration, seconds}
      \item{origyr}{origin time for epoch calculations}
      }


}
\value{

\item{yr}{start year}
\item{jd}{start julian day}
\item{t1}{start t1 (with epoch) }
\item{t2}{start t2 (with epoch day) }
\item{ed}{ epoch day}
\item{hr}{start  hour}
\item{mi}{start  minute}
\item{sec}{start  seconds}
\item{gamp}{Amplitude of each trace}
\item{gdt}{delta-t, sample interval, in seconds}
\item{gnam}{station name}
\item{gfile}{file information}
\item{sigs}{List of time series}
\item{zna}{List of NA values in each time series}

  
}

\author{
Jonathan M. Lees<jonathan.lees@unc.edu>
}


\seealso{
  setupDB, plotseis24
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
####   each trace in a separate file
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
