\name{setstas}
\alias{setstas}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Set Station information}
\description{
  Read station information and set in list
}
\usage{
setstas(stafile)
}
%- maybe also 'usage' for other objects documented here.
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

\dontrun{ sta = setstas("/pathto/stationfile") }
}


\author{Jonathan M. Lees<jonathan.lees.edu>}

\keyword{misc}
