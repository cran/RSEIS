\name{ZGET.sac}
\alias{ZGET.sac}
\title{Get single SAC file}
\description{Get single SAC file from binary data
}
\usage{
ZGET.sac(infile, PLOT = FALSE)
}
\arguments{
  \item{infile}{path name to file}
  \item{PLOT}{logical, plot (not used) }
}
\details{
  Program uses C-code to read in binary SAC files
  and returns the header information and the time series data 
  
}
\value{
 LIST:
    \item{x}{vector of amplitudes}
    \item{mints}{vector if integer SAC header values}
    \item{dubs}{vector if float(double) SAC header values}
    \item{mchars}{vector if 8-character SAC header values}
    \item{echars}{vector if 16-character  SAC header values}
    \item{dubnames}{names of float values}
    \item{mintnames}{names of integer values}
    \item{charnames}{names of char values}
}

\author{Jonathan M. Lees<jonathan.lees@unc.edu>}
\note{

  Sometimes information in the SAC header is omitted by the user
  and instead stored in the file name.  In that case the
  station name and component name will have to be replaced
  by the appropriate string from the file name.

  
}
\seealso{dogetsac, JGET.seis, Package:Rsac}
\examples{

\dontrun{

###  you must replace this file name with local SAC file name
infile='VLI.BHZ.SAC'

ZZ = ZGET.sac(infile)

 dt = ZZ$dubs[ which(ZZ$dubnames=='delta') ]

        yr = ZZ$mints[which(ZZ$mintnames=="nzyear")]
        jd= ZZ$mints[which(ZZ$mintnames=="nzjday")]
        hr=  ZZ$mints[which(ZZ$mintnames=="nzhour")]
        mi=  ZZ$mints[which(ZZ$mintnames=="nzmin")]
        sec= ZZ$mints[which(ZZ$mintnames=="nzsec")]
        msec=ZZ$mints[which(ZZ$mintnames=="nzmsec")]


        sec = sec+msec/1000
      DATTIM=list(yr=yr, jd=jd, hr=hr, mi=mi, sec=sec)

###  the SAC file does not have the correct names, so we read from the
###              input file names
sta = substr(infile, 1,3)
comp = substr(infile, 7,7)

}

}
\keyword{misc}
