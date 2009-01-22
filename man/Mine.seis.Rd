\name{Mine.seis}
\alias{Mine.seis}

\title{Mine a seismic data base to extract secions of time limited data}
\description{
 Mine a seismic data base to extract secions of time limited data
}
\usage{
Mine.seis(at1, at2, DB, grepsta, grepcomp, kind = 1, CHOP=TRUE)
}

\arguments{
  \item{at1}{time 1 in julian days}
  \item{at2}{time 2 in julian days}
  \item{DB}{data base structure to searcth through that provides the
    files where data is to extracted from}
  \item{grepsta}{which stations to extract}
  \item{grepcomp}{which components to extract}
  \item{kind}{kind of data, segy=1, sac=2}
  \item{CHOP}{cut the data to a window using CHOP.SEISN }
  
}
\details{
  The data base is a list or dataframe containing the files names, the
  beginning time (t1) and ending time (t2) for each file in the data
  base.  Mine.seis uses grep on the file names to
  extract specific files from the DB list.
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
\seealso{GLUEseisMAT, JGET.seis}
\keyword{misc}
