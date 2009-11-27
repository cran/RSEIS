\name{parseFN2STA}
\alias{parseFN2STA}

\title{get station from file name}
\description{
  station and component are assumed to be the last elements
  of a file name - this function returns a list with these text strings.
  
}
\usage{
parseFN2STA(fn, ista, icomp, sep="\\\\."  )
}

\arguments{
  \item{fn}{text file name }
  \item{ista}{index of station name counting from the end of the file name }
  \item{icomp}{index of station name counting from the end of the file name }
  \item{sep}{seperator token in file name }
      
}
\details{
  Some format store the station in the file name
  rather than the seismic header.
  Default assumes that the station name and the component name
are the last  items on the file name seperated by a period.
So ista and icomp are computed from the end of the file name,
i.e. ista=1 and icomp=0.

}
\value{
 list(sta='text station name', comp='compname')
}

\author{Jonathan M. Lees<jonathan.lees@unc.edu>}

\examples{

parseFN2STA('/data/wadati/bourbon/GUATEMALA/SEGY/R009.01/07.009.22.25.34.CAS.E')


}

\keyword{misc}
