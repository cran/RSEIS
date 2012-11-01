\name{EPOCHday}
\Rdversion{1.1}
\alias{EPOCHday}
\title{Epoch Day}
\description{Number of days since Origin  Year}
\usage{
EPOCHday(yr, jd = 1, origyr = 1972)}
\arguments{
  \item{yr}{year}
  \item{jd}{Julian Day}
  \item{origyr}{origin year, default=1972}
}
\details{
  Either jd or mo, dom can be provided
}
\value{
  List:
  \item{jday}{number of days since the start of origin year}
  \item{origyr}{origin year used}
  
}
\author{Jonathan M. Lees<jonathan.lees.edu>}
\seealso{EPOCHyear, recdate}
\examples{

tyears   <-  1973:2009

E1  <-  EPOCHday(tyears, jd=1,   origyr=1972       )

EPOCHyear(E1$jday,   origyr=1972  )


}
\keyword{misc}
