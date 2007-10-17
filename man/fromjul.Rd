\name{fromjul}
\alias{fromjul}

\title{given julian day and year get month/day}
\description{
 given julian day and year get month/day
}
\usage{
fromjul(jul, yy)
}

\arguments{
  \item{jul}{Julian Day}
  \item{yy}{year}
}

\value{
  list(mo=mm, dom=dd)
}
\examples{

iyear = 2001
 jul = 233
inine =   tojul(iyear,1,1);
ijul =    inine + jul - 1;
fromjul( ijul, iyear);


}


\author{Jonathan M. Lees<jonathan.lees.edu>}

\seealso{tojul}

\keyword{misc}