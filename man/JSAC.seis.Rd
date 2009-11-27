\name{JSAC.seis}
\alias{JSAC.seis}
\alias{JSEGY.seis}

\title{JSAC.seis
}
\description{Read SEGY/SAC format binary data
}
\usage{
JSAC.seis(fnames, Iendian = 1 , HEADONLY=FALSE, BIGLONG=TRUE, PLOT = FALSE)
JSEGY.seis(fnames, Iendian = 1 , HEADONLY=FALSE, BIGLONG=TRUE, PLOT = FALSE)

}
\arguments{
  \item{fnames}{vector of file names to be extracted and converted.
  }
  
\item{Iendian}{ Endian-ness of the data: 1,2,3: "little", "big", "swap".
  Default = 1 (little)
}
\item{HEADONLY}{ logical, TRUE= header information only
}
\item{BIGLONG}{ logical, TRUE=long=8 bytes
}


  \item{PLOT}{logical, whether to plot the data after reading in
  }
  
}
\details{
  Uses readBin to extract data in SAC format.
  user must know what kind of machine the data
  was created on for I/O purposes.
  
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

\author{
Jonathan M. Lees<jonathan.lees@unc.edu>
}
\note{

  SAC created on PC (windows) or LINUX machines typically will be
  in little endian format.
  SAC created on a SUN will be in big format.
  If you want to swap endian-ness , choose swap.

  MAC uses different convention.

  There are other issues regardin the size of long.
  These will be dealt with in future implementations.

  

  
}


\seealso{Mine.seis, Package:Rsac
}
\examples{

\dontrun{

Lname = list.files(path='/data/wadati/bourbon/LaurelCanNC/R005.01' , pattern="08.005.01.41.23.9024", full.names=TRUE)

S1  = JSAC.seis(Lname, Iendian = 1, PLOT = FALSE)


}




}
\keyword{misc}
