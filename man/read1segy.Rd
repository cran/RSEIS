\name{read1segy}
\alias{read1segy}
\alias{read1sac}

\title{Read one SEGY/SAC file
}
\description{Read one SEGY/SAC file
}
\usage{
read1segy(fname, Iendian = 1, HEADONLY = FALSE, BIGLONG = FALSE)
read1sac(fname, Iendian = 1, HEADONLY = FALSE, BIGLONG = FALSE )

}
\arguments{
  \item{fname}{character, file name
}
  \item{Iendian}{Endian of the input file name
}
  \item{HEADONLY}{logical, TRUE=return only header (default=FALSE)
}
  \item{BIGLONG}{logical, indicating whether long is 8 or 4 bytes.
}
}
\details{Segy format files are in integer format.
  The time series ususally represents counts
  recorded in a data acquisition system.  The header includes
  meta-data and other identifying information.

  SAC data is stored as floats, typically volts.
}
\value{list of header and times series
}

\author{
Jonathan M. Lees<jonathan.lees@unc.edu>
}
\note{
   The Endian-ness of the input files is set by
  the system that created them.
  If the read1segy or read1sac does not make sense,
  try a different endian or BIGLONG setting.

  
}


\seealso{write1sac, write1segy, sac2rseis, segy2rseis, prepSEIS
}
\examples{
\dontrun{

fnames = list.files(path="/Users/lees/Site/Buf2014/ALL_SEGY",
                                     pattern=NULL, full.names=TRUE )

for(i in 1:length(fnames))
{
fn1  = fnames[i]

bed =  read1segy(fn1   , Iendian = 1 , HEADONLY=TRUE, BIGLONG=FALSE)
scaly[i] =  bed$HEAD$scale_fac
print(paste(i, fn1, bed$HEAD$station_name , bed$HEAD$channel_name,
       bed$HEAD$scale_fac, bed$HEAD$gainConst)  )

}


}

}
\keyword{misc}
