\name{Mine.seis}
\alias{Mine.seis}

\title{Mine a seismic data base to extract secions of time limited data}
\description{
 Mine a seismic data base to extract secions of time limited data
}
\usage{
Mine.seis(at1, at2, DB, grepsta, grepcomp, kind = 1, Iendian=1,
BIGLONG=FALSE, CHOP=TRUE, verbose=FALSE, chtoken=NULL, statoken=NULL, RAW=FALSE)
}

\arguments{
  \item{at1}{time 1 in julian days}
  \item{at2}{time 2 in julian days}
  \item{DB}{data base structure to searcth through that provides the
    files where data is to extracted from}
  \item{grepsta}{which stations to extract}
  \item{grepcomp}{which components to extract}
  \item{kind}{kind of data, -1="RDS", 0="RDATA" ,  0="RDATA", 1 = "segy",
    2 = "sac"}

\item{Iendian}{ Endian-ness of the data: 1,2,3: "little", "big", "swap".
  Default = 1 (little)
}

\item{BIGLONG}{ logical, TRUE=long=8 bytes
}


  
  \item{CHOP}{cut the data to a window using CHOP.SEISN }
  \item{verbose}{print out intermediate information for debugging }
   \item{chtoken}{channel token for selecting channels (NULL) }
   \item{statoken}{station token for selecting stations (NULL) }
    \item{RAW}{logical, default=FALSE(convert to volts) , TRUE (return counts intead of
      volts) }
}
\details{
  
  The data base is a list or dataframe containing the files names, the
  beginning time (t1) and ending time (t2) for each file in the data
  base.  Mine.seis uses grep on the file names to
  extract specific files from the DB list.

  Mine.seis needs to know what format the data was
  created in: little/big endian and the size of the LONG.

 
  If data was created on a little endian machine but
  is being read on big endian machine, need to call
  the endian "swap" for swapping.

  If data was created on a machine with LONG=4 bytes,
  be sure to call the program with BIGLONG=FALSE.

  Use sysinfo to findout the system parameters for the local
  system.  You need to know, however, what machine the binary files
  were created on.

  
 In some situation the chanel name and the station name are not embedded in the
    file headers - in that case use the token from the file name.

  
  }
  
\note{

  
  The headers in the digital (segy or SAC) data files
  may not necessarily match the file names.  Note that
  program JGET.seis extracts the station name and component
  name from the digital header and does not use the
  file name.  It may be prudent to force the file names
  and header files to match prior to using Mine.seis.
  For SEGY files, in LINUX-UNIX, use:
  rename,    segymod (PASSCAL) to modify the headers.

  For SAC files, use sac software.

  For R-based codes save the files in
  a format that has the relevant information (DAT format).


  
}


\value{
  List of seismograms cut from the database
}

\examples{

data(GH)

DD = data.frame(GH$info)

#### get only vertical traces
WV = which( GH$COMPS=='V' )
L1 = length(WV)

GIVE = vector(mode='list')

for(j in 1:L1 )
{
    i = WV[j]
    AA = DD[i,]
    GIVE[[j]] = list(fn = AA$fn, sta =GH$STNS[i] , comp = GH$COMP[i], 
                     dt = AA$dt, DATTIM = AA, N = AA$n1, units = NA, 
                     coords = NA, amp = GH$JSTR[[i]]  )
}
#### par(mfrow=c(length(GIVE) , 1) )
#   for(i in 1:length(GIVE) ) { plotGH(GIVE[[i]])   }
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
IDB = infoDB(DB)


SAMPseis <- Mine.seis(IDB$at1, IDB$at2, DB, IDB$usta[1:3], IDB$ucomp[1], kind = -1   )

w <- swig(SAMPseis, SHOWONLY=0)


}



\author{Jonathan M. Lees<jonathan.lees.edu>}
\seealso{makeDB, GLUEseisMAT, JGET.seis, JSAC.seis, JSEGY.seis, sysinfo}
\keyword{misc}
