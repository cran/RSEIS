\name{makeDB}
\Rdversion{1.1}
\alias{makeDB}
\alias{FmakeDB}


\title{Create a seismic Waveform Database }
\description{Create a seismic Waveform Database}

\usage{
makeDB(path=".", pattern="R", dirs="", kind = 1,
Iendian=1, BIGLONG=FALSE)
FmakeDB(LF2, kind =1, Iendian=1, BIGLONG=FALSE)


}
\arguments{
  \item{path}{character, Path to directory where files and directories exist}
  \item{pattern}{character, pattern for listing of files}
  \item{dirs}{character, vector of directories to be scanned}
  \item{kind}{kind of data: RDS=-1, R(DAT)=0, segy=1; sac=2}
  \item{Iendian}{default=1, Endian-ness of the data: 1,2,3: "little", "big", "swap".
    Default = 1 (little)}
  \item{BIGLONG}{ logical, TRUE means long=8 bytes
  }
  \item{LF2}{ list of files
  }
  


  
}
\details{


  
  The files are typically located in a directory structure
  created by programs like ref2segy, a PASSCAL program for
  downloading data in the field.  Each file contains one seismogram,
  with a header.  makeDB reads in all the headers and
  creates a list of meta-data for later use in RSEIS.

   "kind" can be numeric or character: options are
   'RDS', 'RDATA', 'SEGY', 'SAC', corresponding to (-1, 0, 1, 2).
   

  Uses readBin to extract data in SAC format.
  user must know what kind of machine the data
  was created on for I/O purposes.
  

  If data was created on a little endian machine but
  is being read on big endian machine, need to call
  the endian "swap" for swapping.

  If data was created on a machine with LONG=4 bytes,
  be sure to call the program with BIGLONG=FALSE.

  If the base directory, or the subdirectories,  contain files that are
  not seismic data then care must be taken.  Perhaps use FmakeDB
  to explicitly names the files for the DataBase.

  If using FmakeDB a simple vector of files
  (full path names) should be provided.

  The origin year, used for getting the Epoch year,
  is stored as attribute origyr.
  

  
}
\value{
  list:
   \item{fn}{file name}
      \item{yr}{year}
      \item{jd}{julian day}
      \item{hr}{hour}
      \item{mi}{minute}
      \item{sec}{second}
      \item{dur}{duration, seconds}
      \item{t1}{time 1 in Epoch days}
     \item{ t2}{time 2 in Epoch days}
      \item{sta}{station name}
      \item{comp}{component name}
      \item{dt}{sample rate, seconds}
}

\author{Jonathan M. Lees<jonathan.lees@unc.edu>}
\note{
  Epoch times are used to accomodate problems where
  julian days cross year end boundaries, so that
  day 366 comes before day 1 of the next year.

  The origyr, kind , Iendian, BIGLONG are stored as attributes
  in the Database.
  
}

\seealso{setupDB, Mine.seis , getseis24, plotseis24, EPOCHday, swig}
\examples{
\dontrun{

#####  set directory
path <- '/home/lees/Site/Santiaguito/SG09'
pattern <- "R0*"

###   get DB information
XDB  <-  makeDB(path, pattern, kind =1)

##### select a station
usta <- "CAL"
acomp <- "V"

#####   extract 24 hours worht of data
JJ <- getseis24(DB, 2009, 2, usta, acomp, kind = 1)


#####  plot 24 hours worth of data

pjj <- plotseis24(JJ, dy=1/18, FIX=24, SCALE=0,
FILT=list(ON=FALSE, fl=0.05 , fh=20.0, type="BP", proto="BU"),
RCOLS=c(rgb(0.2, .2, 1), rgb(.2, .2, .2))  )


###  window a small portion on 24 hour display
w <- winseis24(pjj)

###  open a new window
X11()
VNE <- c("V", "N", "E")
###  IJK <- c("I", "J", "K")

gsta <- c("CAL", "KAM", "DOM",  "LAV")

ucomp<-VNE

###  or:   ucomp<-IJK

###   set epoch day and get the times
 eday <- EPOCHday(w$yr, jd = w$jd, origyr = DB$origyr)

##  using epoch day, set times for beginning and end of window
        at1 <- eday$jday + (w$hr[1])/24
        at2 <- eday$jday + (w$hr[2])/24
###  extract data from the data base:
        GH <- Mine.seis(at1, at2, DB, gsta, ucomp)

###  show data:
        swig(GH)

}


}
\keyword{misc}
