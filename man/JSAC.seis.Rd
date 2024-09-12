\name{JSAC.seis}
\alias{JSAC.seis}
\alias{JSEGY.seis}

\title{JSAC.seis
}
\description{Read SEGY/SAC format binary data
}
\usage{
JSAC.seis(fnames, Iendian = 1 , HEADONLY=FALSE,
BIGLONG=FALSE, PLOT = -1, RAW=FALSE)
JSEGY.seis(fnames, Iendian = 1 , HEADONLY=FALSE,
BIGLONG=FALSE, PLOT = -1, RAW=FALSE)
}
\arguments{
  \item{fnames}{vector of file names to be extracted and converted.
  }
  
\item{Iendian}{ vector, Endian-ness of the data: 1,2,3: "little", "big", "swap".
  Default = 1 (little) 
}
\item{HEADONLY}{ logical, TRUE= header information only
}
\item{BIGLONG}{ logical, TRUE=long=8 bytes
}


  \item{PLOT}{integer, <0 no plot; 0 
  interactive; >0 number of seconds to sleep
}
\item{RAW}{logical, default=FALSE(convert to volts) , TRUE (return counts intead of
      volts) }
  
}
\details{
  Uses readBin to extract data in SAC format.
  user must know what kind of machine the data
  was created on for I/O purposes.

  For SEGY data the program is the same, although SEGY data
  does not have the problem of the BIGLONG so that is ignored.

  For either code, a full header is returned, although the
  header for each format may be different.

  
  
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
   
   \item{HEAD}{Full header as a data-frame of values (mixture of float
     and character strings) }

   \item{N}{Number of samples in trace }
   \item{units}{Units of samples, possibly: counts, volts, s, m/s, Pa, etc }

   \item{IO}{list: kind, Iendian, BIGLONG flags for I/O }


  
}

\author{
Jonathan M. Lees<jonathan.lees@unc.edu>
}
\note{

  SAC created on PC (windows) or LINUX machines typically will be
  in little endian format.
  SAC created on a SUN will be in big endian format.
  If you want to swap endian-ness , choose swap.

  MAC uses different convention.

  Iendian can be a vector if input files have different endian-ness.

  SAC inserts -12345  for no data.

  There are other issues regarding the size of long.

  The units are often questionable and depend on the processing.
  The user should be careful and check to see that the
  proper conversions and multipliers have been applied.
  

  
}


\seealso{Mine.seis, rseis2sac
}
\examples{

#####  make some SAC files, then read them in
data(GH)
apath = tempdir()
##  setwd(apath)
## apath = 'TEMP'
J =  rseis2sac(GH, sel =1:5,  path = apath, BIGLONG =FALSE )

####### next read them in
Lname  <-  list.files(path=J , pattern='SAC', full.names=TRUE)

S1   <-  JSAC.seis(Lname, Iendian = .Platform$endian, BIGLONG =FALSE ,  PLOT = -1)

####  check just the first one
i = 1
  plotGH(S1[[i]])


}
\keyword{misc}
