\name{EPOCHyear}
\Rdversion{1.1}
\alias{EPOCHyear}
\title{Epoch Year}
\description{Get year and julian day given number of days since origin}
\usage{
EPOCHyear(iday, origyr = 1972)
}
\arguments{
  \item{iday}{Number of days since origin}
  \item{origyr}{origin year, default=1972 }
}

\value{
  List:
  \item{yr}{Year}
  \item{jd}{Julian day in Year}
}

\author{Jonathan M. Lees<jonathan.lees.edu>}


\seealso{EPOCHday, recdate}

\examples{


tyears  = 1973:2009

E1 = EPOCHday(tyears, jd=1,   origyr=1972       )

EPOCHyear(E1$jday,   origyr=1972  )
E1 = EPOCHday(tyears, mo=12, dom=31,   origyr=1972       )

EPOCHyear(E1$jday,   origyr=1972  )



}
\keyword{misc}
