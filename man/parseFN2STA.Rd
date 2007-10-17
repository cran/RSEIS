\name{parseFN2STA}
\alias{parseFN2STA}

\title{get station from file name}
\description{
  station and component are assumed to be the last elements
  of a file name - this function returns a list with these text strings.
  
}
\usage{
parseFN2STA(fn)
}

\arguments{
   \item{fn}{text file name }
}
\details{
  Some format store the station in the file name
  rather than the seismic header.
  

}
\value{
 list(sta='text station name', comp='compname')
}
\references{ ~put references to the literature/web site here ~ }
\author{Jonathan M. Lees<jonathan.lees@unc.edu>}

\examples{

parseFN2STA('/data/wadati/bourbon/GUATEMALA/SEGY/R009.01/07.009.22.25.34.CAS.E')


}

\keyword{misc}
