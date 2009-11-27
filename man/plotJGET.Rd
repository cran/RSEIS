\name{plotJGET}
\alias{plotJGET}
\title{Plot JGET output
}
\description{Plot JGET output using interactive PICK.GEN
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
  Program combines prepSEIS  and PICK.GEN

  
}
\value{GH list ready for use in other RSEIS programs.
  See prepSEIS for details
}

\author{
Jonathan M. Lees<jonathan.lees@unc.edu>
}


\seealso{JGET.seis, prepSEIS, PICK.GEN
}
\examples{

\dontrun{

fn = '/home/lees/taurus_2050_20100302_100000_BHE.sac'

J  = JGET.seis(fn,kind=2,BIGLONG=FALSE,HEADONLY=FALSE,Iendian=3,PLOT=TRUE)

plotJGET(J)

}



}
\keyword{iplot}
