\name{JGET.seis}
\alias{JGET.seis}
\alias{getseisinfo}

\title{Reads various seismic file formats}
\description{
  This fuction calls c-routines to read in \sQuote{segy}, \sQuote{sac}.
}
\usage{
JGET.seis(fnames, kind = 1, Iendian=1, BIGLONG=FALSE , HEADONLY=FALSE,  PLOT = FALSE)
getseisinfo(fnames, kind = 1)
}
\arguments{
  \item{fnames}{list of file names.}
  \item{kind}{an integer 1, 2, 3; 0=R(DAT) , 1 = segy, 2 = sac, 3 = AH.}
  \item{Iendian}{ Endian-ness of the data: 1,2,3: "little", "big", "swap".
    Default = 1 (little)
  }
  \item{BIGLONG}{ logical, TRUE=long=8 bytes
  }
  
  
  \item{HEADONLY}{ logical, TRUE= header information only; not seismic
    trace will be returned (runs a little faster).
  }
  
  
  \item{PLOT}{logical, for if the traces should be plotted.  This is
  interactive and can be shut off for large data sets.}
}
\value{
  List containing the seismic data and header information.
  Each trace consists of a list with:
\item{fn}{original file name}
\item{sta}{station name}
\item{comp}{compnent}
\item{dt}{delta t in seconds}
\item{DATTIM}{time list}
   \item{yr}{year}
   \item{jd}{julian day}
   \item{mo}{month}
   \item{dom}{day of month}
   \item{hr}{hour}
   \item{mi}{minute}
   \item{sec}{sec}
   \item{msec}{milliseconds}
   \item{dt}{delta t in seconds}
   \item{t1}{time start of trace}
   \item{t2}{time end of trace}
   \item{off}{off-set}
   \item{N}{number of points in trace}
   \item{units}{units}
   \item{amp}{vector of trace values}
   
 }
 \details{
 
  Uses readBin to extract data in SAC format.
  user must know what kind of machine the data
  was created on for I/O purposes.
  

  If data was created on a little endian machine but
  is being read on big endian machine, need to call
  the endian "swap" for swapping.

  If data was created on a machine with LONG=4 bytes,
  be sure to call the program with BIGLONG=FALSE.

  See examples below for different cases.
  



   
   }
\seealso{JSAC.seis , Mine.seis}
\note{


  OLDER:
  
  getseisinfo gets only the header information from the trace.
  Information in the file names is ignored, so be sure to modify headers
  prior to using this method of extracting meta-data. (Or modify
  the meta data from the file names after reading in the data.)
  
   For SEGY files, in LINUX-UNIX, use:
   rename,    segymod (PASSCAL) to modify the headers


  JGET.seis  extracts digital seismic data from binary files
   stored in the file system.  The program uses readBin for I/O
   and passes data back to R.  Currently SAC, SEGY formats
   are installed but it is easy to extend.  AH format
   is available for LINUX systems, but there were problems compiling in
   WINDOWS and MACOS so this feature was removed.
   A filter for mseed format is currently being developed.

   getseisinfo is an older  (superceded) C-coded version to get the
   headers.



   
}



\author{Jonathan M. Lees <jonathan.lees@unc.edu>}


\examples{  
\dontrun{

Lname = list.files(path='/data/wadati/bourbon/LaurelCanNC/R005.01' , pattern="08.005.01.41.23.9024", full.names=TRUE)

S1  = JGET.seis(Lname, kind = 1, PLOT = FALSE)

###   for data created on UNIX (SUN) but read on linux:

S1  = JGET.seis(Lname, kind = 1, Iendian="swap", BIGLONG=FALSE, PLOT = FALSE)

###   for data created on linux (32 bit)  but read on linux 64 bit:

S1  = JGET.seis(Lname, kind = 1, Iendian="little", BIGLONG=FALSE, PLOT = FALSE)

###   for SEGY data created on linux (64 bit)  but read on linux 32 bit:

S1  = JGET.seis(Lname, kind = 1, Iendian="little", BIGLONG=TRUE, PLOT = FALSE)


###   for SAC data created on MAC-OS (64 bit)  but read on linux 32 bit:

S1  = JGET.seis(Lname, kind = 2, Iendian="swap", BIGLONG=TRUE, PLOT = FALSE)





}


}
\keyword{misc}
