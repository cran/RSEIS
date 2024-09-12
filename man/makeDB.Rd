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


##########  to illustrate, we make a set of individual seismograms
data(GH)
L1 = length(GH$JSTR)
DD = data.frame(GH$info)

GIVE = vector(mode='list')

for(i in 1:L1)
{
AA = DD[i,]
GIVE[[i]] = list(fn = AA$fn, sta =GH$STNS[i] , comp = GH$COMP[i], 
            dt = AA$dt, DATTIM = AA, N = AA$n1, units = NA, 
            coords = NA, amp = GH$JSTR[[i]]  )
}

###########  save the seismic data in a temporary directory
####   each trace in a separate file
tdir = tempdir()
for(i in 1:length(GIVE) )
{
    sig = GIVE[[i]]
  d1 =  dateStamp(sig$DATTIM, sep='_')
   nam1 =  paste(d1,sig$sta, sig$comp, sep='_')
    nam2 = paste0(nam1, '.RDS')
    nam3 = paste(tdir, nam2, sep='/')
    saveRDS(file=nam3, sig) 
    }

########################  Now  read files and make the DataBase:
LF  = list.files(path=tdir,pattern='.RDS',  full.names=TRUE)
DB = FmakeDB(LF, kind=-1)


}
\keyword{misc}
