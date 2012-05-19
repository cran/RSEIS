\name{rseis2sac}
\alias{rseis2sac}
\title{Dump RSEIS list to SAC format
}
\description{
  Write a set of SAC format files from an RSEIS
  list of time series.
  
}
\usage{
rseis2sac(GH, sel = 1, win = c(0, 1), dir = ".")
}
\arguments{
  \item{GH}{RSEIS list
}
  \item{sel}{integer, index to select traces in RSEIS-list
}
  \item{win}{vector, start and end times to dump out (not active) 
}
  \item{dir}{character, path to destination directory
}
}
\details{
  Dumps out each trace individually as a SAC file in the destination
  directory. This is useful for sharing data with researchers
  who refuse to see the value of R.  For the inverse operation (reading
  in SAC files from disk, use the RSEIS code, JSAC.seis.
}
\value{
  Side effects, creates files.
}
\references{see :
http://www.iris.edu/software/sac/manual/file_format.html
  
}
\author{
Jonathan M. Lees<jonathan.lees@unc.edu>
}
\note{
  Only the bare essentials of the SAC file are
  converted.  
}


\seealso{write1sac,  JSAC.seis
}
\examples{

\dontrun{

data(GH)
rseis2sac(GH )

}




}
\keyword{misc}
