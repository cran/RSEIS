\name{parseFN2STA}
\alias{parseFN2STA}

\title{get station from file name}
\description{
  station and component are assumed to be the last elements
  of a file name - this function returns a list with these text strings.
  
}
\usage{
parseFN2STA(fn, ista, icomp, sep="\\\\.", dir=0  )
}

\arguments{
  \item{fn}{text file name }
  \item{ista}{index of station name counting from the end of the file name }
  \item{icomp}{index of station name counting from the end of the file name }
  \item{sep}{separator token in file name }
  \item{dir}{integer, default=0, direction for counting. see details }
  
      
}
\details{
  Some seismic data formats store the station in the file name
  rather than the seismic header.
  The default (dir=0) assumes that the station name and the component name
are the last items on the file name seperated by a period.
So ista and icomp are computed from the end of the file name,
i.e. ista=1 and icomp=0.
If (dir=1) the counting is from the beginning
of the string and the count starts at 1.
Remember to count double tokens, they return a blank.


}
\value{
 list(sta='text station name', comp='compname')
}

\author{Jonathan M. Lees<jonathan.lees@unc.edu>}

\examples{

parseFN2STA('/data/wadati/bourbon/GUATEMALA/SEGY/R009.01/07.009.22.25.34.CAS.E')

fn  <- "2011-11-06-0637-21S.SI01__003_SI01__SH_N_SAC"

parseFN2STA(fn, 4, 1, sep="_"  )
### or:
parseFN2STA(fn, 4, 7, sep="_", dir=1  )


}

\keyword{misc}
