\name{view.seis}
\alias{view.seis}
\title{Veiw seismic data window}
\description{Veiw seismic data (segy) window on an hourly basis.
}
\usage{
view.seis(aday, ihour, inkhour, SAVEFILE, days,
DB, usta, acomp,
STDLAB =c("QUIT",  "NEXT", "PREV", "HALF"),
kind = -1, Iendian=1, BIGLONG=FALSE,
TZ=NULL)
}
\arguments{
  \item{aday}{index of which day to use in vector days }
  \item{ihour}{hour to start }
  \item{inkhour}{increment in hours for viewing panel }
  \item{SAVEFILE}{file to save window picks in }
  \item{days}{vector of days to select from }
  \item{DB}{data base list of file names and start-times and durations }
  \item{usta}{stations to select }
  \item{acomp}{compnents to select }
  \item{STDLAB}{vector of buttons,  DEFAULT = c("QUIT",  "NEXT", "PREV", "HALF",  "WPIX", "zoom out",
         "refresh", "restore",  "SPEC", "SGRAM" ,"WLET", "FILT",
         "Pinfo", "WINFO") }
  \item{kind}{an integer -1, 0, 1, 2 ; 0="RDATA" , -1="RDS", 0="RDATA", 1 = "segy",
    2 = "sac", see notes below}
  \item{Iendian}{ vector,   Endian-ness of the data: 1,2,3: "little", "big", "swap".
    Default = 1 (little)
  }
  \item{BIGLONG}{ logical, TRUE=long=8 bytes
  }
   
  \item{TZ}{Number of hours to add to GMT to get local time}
}
\details{
  The program view.seis assumes the data is stored in files accessable
  by the user and that the DB list has been scanned
  in and parsed.

   "kind" can be numeric or character: options are
   'RDS', 'RDATA', 'SEGY', 'SAC', corresponding to (-1, 0, 1, 2)
   

  
}
\value{
 Graphical side effects and save.wpix stores appended picks.
}

\author{Jonathan M. Lees<jonathan.lees@unc.edu>}
\note{
  On LINUX systems I wrote these (non-R) programs
  to set up the data base for segy data:FLS.prl, segydatabase.
  To get these contact me directly.  TZ is (-6) for Guatemala.
}
\seealso{swig, save.wpix}
\examples{

if(interactive() ) {

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

pday <- 5
SAVEFILE <- tempfile()
ihour <- 15
inkhour <- .5

 ###  days is a list of days (and associated years) that are in teh DB
     days <- list(jd=c(4, 5, 6), yr=c(2000, 2000, 2000) )
     aday = which(pday == days$jd)


####   aday refers to one of the days listed in the days structure


view.seis(aday, ihour, inkhour, SAVEFILE, days, DB, IDB$usta, IDB$ucomp, TZ=(-6))


}


}
\keyword{misc}
