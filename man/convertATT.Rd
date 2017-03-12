\name{convertATT}
\alias{convertATT}
\title{DateHour to List
}
\description{Convert a julian day+time to an RSEIS date list.
}
\usage{
convertATT(at1, yr)
}
\arguments{
  \item{at1}{julian day in Year, plus (hr+minutes+seconds)
}
  \item{yr}{Year
}
}
\details{
Calculates the data-list that RSEIS uses in calculations.  The Month and Day-of-month are also returned.
}
\value{
List with date and time
}

\author{
Jonathan M. Lees<jonathan.lees@unc.edu>
}


\seealso{recdate, recdatel, dateList, dateStamp, filedatetime,
rangedatetime, yeardate, Zdate, as.POSIXct
}
\examples{

yr = 2014
j = 233.1234
convertATT(j, yr)

}
\keyword{misc}
