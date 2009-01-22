\name{getmem}
\alias{getmem}

\title{Get Member}
\description{
  Get a member of a list
}
\usage{
getmem(v, mem = 1)
}

\arguments{
  \item{v}{vector}
  \item{mem}{element in vector}
}
\details{
  Used in conjunction with apply
}
\value{
vector of members of a list
}

\author{Jonathan M. Lees<jonathan.lees@unc.edu>}

\examples{

z = list()
for(i in 1:10)
{

z[[i]] = round(10*runif(10))


}

y = as.vector(unlist(lapply(z, getmem, 6)))



}


\keyword{misc}

