\name{GET.seis}
\alias{GET.seis}
\alias{JGET.seis}

\title{Reads various seismic file formats}
\description{
  This fuction calls binary routines to read in \sQuote{segy}, \sQuote{sac}.
}
\usage{
GET.seis(fnames, kind = 1, Iendian=1, BIGLONG=FALSE ,
HEADONLY=FALSE,  PLOT = -1, RAW=FALSE)

JGET.seis(fnames, kind = 1, Iendian=1, BIGLONG=FALSE ,
HEADONLY=FALSE,  PLOT = -1, RAW=FALSE)

}
\arguments{
  \item{fnames}{list of file names.}
  \item{kind}{an integer 1, 2, 3; 0=R(DAT) , 1 = segy, 2 = sac, 3 = AH.}
  \item{Iendian}{ vector,   Endian-ness of the data: 1,2,3: "little", "big", "swap".
    Default = 1 (little)
  }
  \item{BIGLONG}{ logical, TRUE=long=8 bytes
  }
  
  
  \item{HEADONLY}{ logical, TRUE= header information only; not seismic
    trace will be returned (runs a little faster).
  }
  
  
  \item{PLOT}{integer, <0 no plot; 0 
    interactive; >0 number of seconds to sleep}
  \item{RAW}{logical, default=FALSE(convert to volts) , TRUE (return counts intead of
      volts) }
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
 
  Uses readBin to extract data in SAC/SEGY format.
  User must know what kind of machine the data
  was created on for I/O purposes.
  

  If data was created on a little endian machine but
  is being read on big endian machine, need to call
  the endian "swap" for swapping.

   Iendian can be a vector if input files have different endian-ness.


  If data was created on a machine with LONG=4 bytes,
  be sure to call the program with BIGLONG=FALSE.

  The data returned is a list of lists, each element is one
  trace not necessarily related to the other traces in the list.
  
  Once the data is read in, use prepSEIS to reformat
  the data into a list more amenable to further analysis in RSEIS.

  See examples below for different cases.
  

  


   
   }
\seealso{plotJGET, JSAC.seis , prepSEIS, Mine.seis}
\note{


  OLDER:
  
  
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

   



   
}



\author{Jonathan M. Lees <jonathan.lees@unc.edu>}


\examples{  
\dontrun{

Lname  <-  list.files(path='/data/wadati/bourbon/LaurelCanNC/R005.01' ,
         pattern="08.005.01.41.23.9024", full.names=TRUE)

S1   <-  GET.seis(Lname, kind = 1, PLOT = -1)

###   for data created on UNIX (SUN) but read on linux:

S1   <-  GET.seis(Lname, kind = 1, Iendian="swap", BIGLONG=FALSE, PLOT = -1)

###   for data created on linux (32 bit)  but read on linux 64 bit:

S1   <-  GET.seis(Lname, kind = 1, Iendian="little", BIGLONG=FALSE, PLOT = -1)

###   for SEGY data created on linux (64 bit)  but read on linux 32 bit:

S1   <-  GET.seis(Lname, kind = 1, Iendian="little", BIGLONG=TRUE, PLOT = -1)


###   for SAC data created on MAC-OS (64 bit)  but read on linux 32 bit:

S1   <-  GET.seis(Lname, kind = 2, Iendian="swap", BIGLONG=TRUE, PLOT = -1)


##################   



##########    to use other parts of the seismic
############               analysis package convert to rseis format:


####  prepare the data for further processing:

S1   <-  JGET.seis(Lname, kind = 2, Iendian="swap", BIGLONG=TRUE, PLOT = -1)

 GH <- prepSEIS(S1)
####   plot the data, and interact with the data
swig(GH, SHOWONLY=0)

###  or simply:

plotJGET(S1)

}


}
\keyword{misc}
