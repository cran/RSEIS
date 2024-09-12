\name{write1segy}
\alias{write1segy}
\alias{write1sac}


\title{Write One segy/sac  file
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
 theENDIAN =  .Platform$endian
BIGLONG = FALSE
###  write1segy is in rseis2segy
data(KH)
apath = tempdir()
J = rseis2segy(KH, sel=1, path=apath, BIGLONG=BIGLONG )
L = list.files(path=J, full.names=TRUE)

Z = read1segy(L[1], Iendian = theENDIAN, HEADONLY = FALSE, BIGLONG = BIGLONG)
plot(Z$amp, type='l')

##########   same with SAC files:
J =  rseis2sac(KH, sel = 1, win = c(0, 1), path = apath, BIGLONG = BIGLONG)
L = list.files(path=J, pattern='.SAC', full.names=TRUE)

Z = read1sac(L[1], Iendian = theENDIAN, HEADONLY = FALSE, BIGLONG = BIGLONG)

plot(Z$amp, type='l')

}


}
\keyword{misc}
