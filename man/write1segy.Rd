\name{write1segy}
\alias{write1segy}
\alias{write1sac}


\title{Write One segy file
}
\description{
  Write out one segy binary format file.
}
\usage{
write1segy(alist, fn = NULL, BIGLONG = FALSE)
write1sac(alist, fn = NULL, BIGLONG = FALSE)

}
\arguments{
  \item{alist}{
    list of traces with  segy/sac header and an integer/real format time series
}
\item{fn}{
  Output file name
}
\item{BIGLONG}{
  logical, indicating whether long is 8 or 4 bytes.
}
}
\details{
  Segy format files are in integer format.
  The time series ususally represents counts
  recorded in a data acquisition system.  The header includes
  meta-data and other identifying information.
  
}
\value{Side effects in the file system.
}

\author{
Jonathan M. Lees<jonathan.lees@unc.edu>
}
\note{
  The Endian-ness of the output file will
  be the native endian-ness of the system.
}


\seealso{rseis2segy, read1sac, read1segy
}
\examples{
\dontrun{
tdir = tempdir()

bed =  read1segy(fn1   , Iendian = 1 , HEADONLY=FALSE, BIGLONG=FALSE)

haname =  write1segy(bed  , fn=paste(tdir,"jimmy.SEGY", sep='/') , BIGLONG=FALSE)

}

}
\keyword{misc}
