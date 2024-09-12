\name{longfft}
\alias{longfft}
\alias{longpstart}
\alias{longreset}

\title{Long FFT Spectrogram
}
\description{Creates hourly spectrograms,
  either alternating seismic and infrasound data or
  sequences of one component. 
}
\usage{
longfft(DB, DAYS = c(233, 234), HRS = 1:24, sta = "KR1", comp = c("V",
"I"), NPP = 6, CSCALE = FALSE, pal = rainbow(100), PS = FALSE, kind = 1,
Iendian = 1, BIGLONG = FALSE)

longreset(NPP, PS)

longpstart(NPP = 6, asta = "", acomp = "", theday = 1, hr = 0)

}
\arguments{
  \item{DB}{RSEIS Data base
}
  \item{DAYS}{vector of Days to display 
}
  \item{HRS}{vector of hours to display
}
  \item{sta}{stations to extract
}
  \item{comp}{component to extract
}
  \item{NPP}{Number of plot strips per page, default = 6
}
  \item{CSCALE}{scaling
}
  \item{pal}{palettes to use (given two will alternate these) 
}
  \item{PS}{logical, TRUE postscript output
}
  \item{kind}{data type, an integer -1, 0, 1, 2 ; 0=R(DAT) , -1=RDS, 0=RDATA, 1 = segy,
    2 = sac
}
  \item{Iendian}{Endian-ness of binary data
}
  \item{BIGLONG}{logical, TRUE=long is 8 bytes
  }

   \item{asta}{character, one station
   }
   

    \item{acomp}{character, one component
}
  \item{theday}{one day
}
  \item{hr}{one hour
}
}
\details{

  Extracts dats from the DB data base
  and plots strips of spectrograms for perusal.

  longpstart, longreset are auxilliary codes used to set up the
  postscript files and initialize the plotting.
  
}
\value{
  Graphical Side effects
}

\author{
Jonathan M. Lees<jonathan.lees@unc.edu>
}
\note{

  Program is set for data being ready from external
  sources in binary (SAC, SEGY) format.
  If data is in R-format already, the code may
  not work.
  
}

\seealso{SPECT.drive
}
\examples{

if(interactive()){
 #####  get a time series
     data(KH)
     
     amp = KH$JSTR[[1]]
     OLDdt = KH$dt[1]
     ####  downsample to:
     newdt = 0.1
     
     JK = FAKEDATA(amp, OLDdt=OLDdt, newdt = 0.1, yr = 2000,
              JD = 4, mi = 12, sec = 0,  Ntraces = 24,
seed=200, noise.est=c(1, 100) , verbose=TRUE  )
     
    
tdir = tempdir()
for(i in 1:length(JK) )
{
    sig = JK[[i]]
  d1 =  dateStamp(sig$DATTIM, sep='_')
   nam1 =  paste(d1,sig$sta, sig$comp, sep='_')
    nam2 = paste0(nam1, '.RDS')
    nam3 = paste(tdir, nam2, sep='/')
    saveRDS(file=nam3, sig) 
    }

LF  = list.files(path=tdir,pattern='.RDS',  full.names=TRUE)
DB = FmakeDB(LF, kind=-1)
IDB = infoDB(DB)

   p1 <- RPMG::Gcols(plow=5, phi=0,  N=100, pal="topo.colors", mingray=0.8)
     p2 <- RPMG::Gcols(plow=5, phi=0,  N=100, pal="rainbow", mingray=0.8)
    
longfft(DB, DAYS=5 , HRS=1:24 ,
     sta=IDB$usta, comp=IDB$ucomp , NPP=6 , CSCALE=FALSE, 
     pal = list(p1=p1, p2=p2), PS=FALSE ,  kind = -1,
     Iendian=1, BIGLONG=TRUE )


}


}
\keyword{misc}
