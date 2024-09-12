\name{plotJGET}
\alias{plotJGET}
\title{Plot JGET output
}
\description{Plot JGET output using interactive swig
}
\usage{
plotJGET(J, SHOWONLY = FALSE)
}
\arguments{
  \item{J}{list, output of JGETseis
}
  \item{SHOWONLY}{logical, if SHOWONLY== TRUE, no interaction
}
}
\details{
  Program combines prepSEIS  and swig

  
}
\value{GH list ready for use in other RSEIS programs.
  See prepSEIS for details
}

\author{
Jonathan M. Lees<jonathan.lees@unc.edu>
}


\seealso{JGET.seis, prepSEIS, swig
}
\examples{

data(GH)
 Iendian = .Platform$endian
apath = tempdir()
##  setwd(apath)
##
 Iendian = .Platform$endian
##    apath = './TEMP'
### dir.create(apath)

J =  rseis2sac(GH, sel = 1:5, path = apath, BIGLONG =FALSE )

Lname  <-  list.files(path=J , pattern='SAC', full.names=TRUE)

J  <- JGET.seis(Lname,kind=2,BIGLONG=FALSE,HEADONLY=FALSE,Iendian=Iendian,PLOT=0)

if(interactive()) { plotJGET(J) }

}
\keyword{iplot}
