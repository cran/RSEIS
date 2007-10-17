\name{yeardate}
\alias{yeardate}

\title{time in decimal years}
\description{
 contract a date to decimal years
}
\usage{
yeardate(yr, jd, hr, mi, sec)
}

\arguments{
  \item{yr}{year}
  \item{jd}{julian day}
  \item{hr}{hour}
  \item{mi}{minute}
  \item{sec}{second}
}

\value{
 decimal time
}

\author{Jonathan M. Lees<jonathan.lees.edu>}

\seealso{secdif}
\examples{

   yeardate(2005, 98, 12, 16, 32)
   
}

\keyword{misc}
