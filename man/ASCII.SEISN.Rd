\name{ASCII.SEISN}
\Rdversion{1.1}
\alias{ASCII.SEISN}
\title{ASCII RSEIS data dump
}
\description{Write RSEIS list to a  file in ASCII format.
}
\usage{
ASCII.SEISN(GH, sel = 1, HEAD = TRUE, destdir='.' )
}
\arguments{
  \item{GH}{RSEIS list}
  \item{sel}{vector, select which ttraces to write}
  \item{HEAD}{logical, TRUE will put a header in the file}
   \item{destdir}{character, path to folder to deposit output file }
 
}
\details{  Used for data exchange for users who do not want to
  use RSEIS.  The header consists of one line
  start date (yr, jd, hr, min, sec) and sample rate (dt).

  
}
\value{
  Side effects - files are created.
}

\author{
Jonathan M. Lees<jonathan.lees@unc.edu>
}

\examples{

\dontrun{
##### this example creates an ascii version of the
#####   seismic data for exchange purposes
data("GH")
tempd = tempdir()
sel <-  which(GH$COMPS == "V" & GH$STNS=="CE1"   )
ASCII.SEISN(GH, sel = 1, HEAD = TRUE, destdir=tempd)
}


}
\keyword{misc}
