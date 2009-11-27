\name{Mine.seis}
\alias{Mine.seis}

\title{Mine a seismic data base to extract secions of time limited data}
\description{
 Mine a seismic data base to extract secions of time limited data
}
\usage{
Mine.seis(at1, at2, DB, grepsta, grepcomp, kind = 1, Iendian=1, BIGLONG=FALSE, CHOP=TRUE, verbose=FALSE)
}

\arguments{
  \item{at1}{time 1 in julian days}
  \item{at2}{time 2 in julian days}
  \item{DB}{data base structure to searcth through that provides the
    files where data is to extracted from}
  \item{grepsta}{which stations to extract}
  \item{grepcomp}{which components to extract}
  \item{kind}{kind of data, segy=1, sac=2}

\item{Iendian}{ Endian-ness of the data: 1,2,3: "little", "big", "swap".
  Default = 1 (little)
}

\item{BIGLONG}{ logical, TRUE=long=8 bytes
}


  
  \item{CHOP}{cut the data to a window using CHOP.SEISN }
  \item{verbose}{print out intermediate information for debugging }
  
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

  

  
}
\note{

  
  The headers in the digital (segy or SAC) data files
  may not necessarily match the file names.  Note that
  program JGET.seis extracts the station name and component
  name from the digital header and does not use the
  file name.  It may be prudent to force the file names
  and header files to match prior to using Mine.seis.
  For SEGY files, in LINUX-UNIX, use:
  rename,    segymod (PASSCAL) to modify the headers


  
}

\value{
list of seismograms cut from the database
}
\author{Jonathan M. Lees<jonathan.lees.edu>}
\seealso{GLUEseisMAT, JGET.seis, JSAC.seis, JSEGY.seis, sysinfo}
\keyword{misc}
