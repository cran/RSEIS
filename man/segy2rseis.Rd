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


#####  make some SAC files, then read them in
data(GH)
apath = tempdir()
##  setwd(apath)
## apath = 'TEMP'
J =  rseis2sac(GH, sel =1:5,  path = apath, BIGLONG =FALSE )
Iendian = .Platform$endian
####### next read them in
Lname  <-  list.files(path=J , pattern='SAC', full.names=TRUE)

H = sac2rseis(Lname , Iendian =Iendian , HEADONLY = FALSE,
BIGLONG = FALSE, PLOT = -1, RAW = FALSE)

####  should have 5 traces, look at elements of the first one:
names(H[[1]])

plotGH(H[[1]])


}
\keyword{misc}
