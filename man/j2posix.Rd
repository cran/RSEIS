\name{j2posix}
\alias{j2posix}
\title{Convert RSEIS date list to Posix
}
\description{Convert RSEIS date list to a cmpatable date/time
for calculating dates and times with base R codes.
}
\usage{
j2posix(timeinput)
}
\arguments{
  \item{timeinput}{ RSEIS date-time list
}
}
\details{
Code here converts to posix, but works only down to the second, i.e.
fractions of a second are dropped.
}
\value{
POSIX compatable date time structure.
}

\author{
Jonathan M. Lees<jonathan.lees@unc.edu>
}
\note{
If you need to preserve the fractional seconds (as we do in seismology)
it is recommended to cut them off and add them later.
}


\seealso{
recdate, recdatel, dateList, dateStamp, filedatetime,
rangedatetime, yeardate, Zdate, as.POSIXct
}
\examples{

yr = 2014
j = 233.1234
A = convertATT(j, yr)
j2posix(A)
### note fractional seconds are truncated.
}
\keyword{misc}
