\name{filedatetime}
\alias{filedatetime}
\title{Create a character string from a date}
\description{
  Create a character string from a date for
  naming unique output files.
}
\usage{
filedatetime(orgtim, tims)
}
\arguments{
  \item{orgtim}{time vector of length 5: c(yr, jd, hr, mi, sec)  }
  \item{tims}{seconds to add to orgtim}
}

\value{
  \item{filename}{character string}
  
}

\author{Jonathan M. Lees<jonathan.lees@unc.edu>}


\examples{

data(GH)

g1 = getGHtime(GH)
g2 = unlist(g1)

filedatetime(g2, 1)



}
\keyword{misc}
