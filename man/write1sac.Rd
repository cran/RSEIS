\name{write1sac}
\alias{write1sac}
\title{Write one SAC file to disk.
}
\description{
  Write one time series trace to a disk.
  The time series must have header information
  available.
}
\usage{
write1sac(a1, BIGLONG = FALSE, fn = NULL)
}
\arguments{
  \item{a1}{
}
  \item{BIGLONG}{logical, TRUE=long=8 bytes
}
  \item{fn}{character, output file name (if null, program will generate
    file name from date-time
}
}
\details{
  Creates a binary SAC format file on disk for sharing and
  transfer.  The header information for the SAC file is fairly limited.
}
\value{
  Side effects, writes file on disk
}
\references{
  http://www.iris.edu/software/sac/manual/file_format.html
  
}
\author{
Jonathan M. Lees<jonathan.lees@unc.edu>
}
\note{
  Header information is rudimentary.
}


\seealso{
  JSAC.seis, rseis2sac
}
\examples{

\dontrun{

data(GH)
theENDIAN <-  .Platform$endian
    for(i in 1:length(GH$dt))
      {
        fn <- "GH"
        thesta <- GH$STNS[i]
        thecomp <- GH$COMPS[i]
        dt <- GH$dt[i]
        
        tstart <- list(yr=GH$info$yr[i]  ,
          jd = GH$info$jd[i]  ,
          mo = GH$info$mo[i]  ,
          dom = GH$info$dom[i]  ,
          hr = GH$info$hr[i]  ,
          mi = GH$info$mi[i]  ,
          sec = GH$info$sec[i]  ,
          msec = GH$info$msec[i]  ,
          dt = GH$info$dt[i]  ,
          t1 = GH$info$t1[i]  ,
          t2 = GH$info$t2[i]  ,
          off = GH$info$off[i]  
          )
        N <- GH$info$n[i]

        sig <- GH$JSTR[[i]]
        
        a1 <- list(fn=fn, sta=thesta,  comp=thecomp, dt=dt, DATTIM=tstart,
          N=N, units=aunits , amp=sig , IO=list(kind=2,
         Iendian=theENDIAN,  BIGLONG=BIGLONG))
        write1sac(a1, BIGLONG=FALSE  , fn=NULL)
      }



}








}
\keyword{misc}
