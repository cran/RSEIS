\name{setstas}
\alias{setstas}

\title{Set Station information}
\description{
  Read station information and set in list
}
\usage{
setstas(stafile)
}

\arguments{
  \item{stafile}{character, station file name path}
}
\details{
  reads in ASCII data file.
}
\value{
 LIST
  \item{name}{character, station name}
  \item{lat}{numeric, decimal degrees}
  \item{lon}{numeric, decimal degrees}
  \item{z}{numeric, decimal degrees}


  
}

\examples{

\dontrun{
sta <- setstas("/pathto/stationfile")
}
}


\author{Jonathan M. Lees<jonathan.lees.edu>}

\keyword{misc}
