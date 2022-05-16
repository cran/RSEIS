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

\dontrun{
###  output directory
setwd("/home/lees/MARIO_TUNG/LONGFFT")

###  2 color palettes
p1 <- Gcols(plow=5, phi=0,  N=100, pal="topo.colors", mingray=0.8)
p2 <- Gcols(plow=5, phi=0,  N=100, pal="rainbow", mingray=0.8)

###   one station, vertical component
longfft(DB, DAYS=c(148:156) , HRS=1:24 ,
sta="BBIL", comp=c("BHZ") , NPP=6 , CSCALE=FALSE, 
pal = list(p1=p1, p2=p2), PS=TRUE ,  kind = 2,
Iendian=1, BIGLONG=FALSE )

###   one station, infrasound
longfft(DB, DAYS=c(148:156) , HRS=1:24 , sta="BBIL",
comp=c("BDF") , NPP=6 , CSCALE=FALSE, 
pal = list(p1=p2, p2=p1), PS=TRUE ,  kind = 2,
Iendian=1, BIGLONG=FALSE )

###  one station, both vertical and infrasound, alternating
longfft(DB, DAYS=c(148:156) , HRS=1:24 , sta="BBIL",
comp=c("BHZ", "BDF") , NPP=6 , CSCALE=FALSE, 
pal = list(p1=p2, p2=p1), PS=TRUE ,  kind = 2,
Iendian=1, BIGLONG=FALSE )





}


}
\keyword{misc}
