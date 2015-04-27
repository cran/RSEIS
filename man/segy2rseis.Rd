\name{segy2rseis}
\alias{segy2rseis}
\alias{sac2rseis}
\title{Read in multiple segy files.
}
\description{Read in multiple segy files,
  and create a list of seismic traces.
}

\usage{
segy2rseis(fnames, Iendian = 1, HEADONLY = FALSE, BIGLONG = FALSE, PLOT
= -1, RAW = FALSE)
sac2rseis(fnames, Iendian = 1, HEADONLY = FALSE,
BIGLONG = FALSE, PLOT = -1, RAW = FALSE)
}
\arguments{
  \item{fnames}{character vector of file names.
}
  \item{Iendian}{Endian-ness of the files
}
  \item{HEADONLY}{logical, TRUE=read only the header information. default=FALSE
}
  \item{BIGLONG}{logical, indicating whether long is 8 or 4 bytes.
}
  \item{PLOT}{logical, TRUE = plot traces 
}
  \item{RAW}{logical, TRUE=do not convert data to volts
}
}
\details{ Segy format files are in integer format.
  The time series ususally represents counts
  recorded in a data acquisition system.  The header includes
  meta-data and other identifying information.
}
\value{
  List of seismic traces.
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


\seealso{read1sac, read1segy, sac2rseis, prepSEIS
}
\examples{
\dontrun{

fnames =
list.files(path="./SEISMIC_DATA/KARYMSKY/1999/kar99/Day254",
pattern="", full.names=TRUE )

MANY = segy2rseis(fnames, Iendian=1 , HEADONLY=FALSE,
    BIGLONG=FALSE,  PLOT=1, RAW=FALSE )


}

}
\keyword{misc}
