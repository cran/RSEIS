\name{ASCII.SEISN}
\Rdversion{1.1}
\alias{ASCII.SEISN}
\title{ASCII RSEIS data dump
}
\description{Write RSEIS list to a  file in ASCII format.
}
\usage{
ASCII.SEISN(GH, sel = 1, HEAD = TRUE)
}
\arguments{
  \item{GH}{RSEIS list}
  \item{sel}{vector, select which ttraces to write}
  \item{HEAD}{logical, TRUE will put a header in the file}
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
data("GH")

sel= which(GH$COMPS == "V" & GH$STNS=="CE1"  )
ASCII.SEISN(GH, sel = 1, HEAD = TRUE)
}
}
\keyword{misc}
