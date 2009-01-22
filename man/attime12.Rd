\name{attime12}
\alias{attime12}
\title{Epoch Time Window}
\description{
  Set a time window in Epoch days for extraction from a DB file
}
\usage{
attime12(t1, t2 = t1, origyr = 1972, pre = 0, post = 0)
}
\arguments{
  \item{t1}{list date-time 1}
  \item{t2}{list date-time 2}
  \item{origyr}{origin year}
  \item{pre}{seconds before time 1}
  \item{post}{seconds afer time 2}
}
\details{
  If t2 is missing, t1=t2.

  
}
\value{
  \item{vector}{c(t1, t2) }
  
}

\author{Jonathan M. Lees<jonathan.lees@unc.edu>}


\examples{

j1 = list(yr=2005, jd= 214 , hr= 7 , mi= 1 ,sec= 0.5235)
j2 = list(yr=2005, jd= 214 , hr= 7 , mi= 1 ,sec= 0.5235+6)

at= attime12(j1, t2=j1, origyr=2005, pre=100, post=100)



}
\keyword{misc}
