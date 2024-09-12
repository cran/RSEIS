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
  \item{kind}{an integer -1, 0, 1, 2 ; 0="RDATA" , -1="RDS", 0="RDATA", 1 = "segy",
    2 = "sac", see notes below}
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

   "kind" can be numeric or character: options are
   'RDS', 'RDATA', 'SEGY', 'SAC', corresponding to (-1, 0, 1, 2).
   
 
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

  The easiext way to process datais to convert the data to
  an R-format type, using either save (kind=0) or saveRDS (kind=-1).
  If these are used then I/O is simple.

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
   
   A filter for mseed format is currently being developed. Could use
   package 'IRISSeismic'

}



\author{Jonathan M. Lees <jonathan.lees@unc.edu>}


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


Gseis = GET.seis(LF, kind = -1, Iendian=1, BIGLONG=FALSE ,
HEADONLY=FALSE,  PLOT = -1, RAW=FALSE)

zed <- prepSEIS(Gseis)

####   plot the data, and interact with the data
swig(zed, sel=which(zed$COMPS=='V'),  SHOWONLY=0)

if(interactive()){ plotJGET(Gseis) }

###   for data created on UNIX (SUN) but read on linux:
###   S1   <-  GET.seis(Lname, kind = 1, Iendian="swap", BIGLONG=FALSE, PLOT = -1)

###   for data created on linux (32 bit)  but read on linux 64 bit:
###  S1   <-  GET.seis(Lname, kind = 1, Iendian="little", BIGLONG=FALSE, PLOT = -1)

###   for SEGY data created on linux (64 bit)  but read on linux 32 bit:
###  S1   <-  GET.seis(Lname, kind = 1, Iendian="little", BIGLONG=TRUE, PLOT = -1)


###   for SAC data created on MAC-OS (64 bit)  but read on linux 32 bit:
###  S1   <-  GET.seis(Lname, kind = 2, Iendian="swap", BIGLONG=TRUE, PLOT = -1)


}
\keyword{misc}
