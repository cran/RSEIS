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

data(GH)
theENDIAN =.Platform$endian

apath = tempdir()
J = rseis2segy(GH, sel=1:5,  path=apath , BIGLONG=FALSE )

Lname  <-  list.files(path=J , pattern='SEGY', full.names=TRUE)

zed = read1segy(Lname[1], Iendian = theENDIAN,
     HEADONLY = FALSE, BIGLONG = FALSE)

}
\keyword{misc}
