\name{jstats}
\alias{jstats}

\title{statistics of a vector}
\description{
returns relevant stats
}
\usage{
jstats(d)
}

\arguments{
  \item{d}{ vector }
}
\details{
Program calls R routines to gather important statistics for later use.
}
\value{
 list:
  \item{mean}{mean value}
  \item{std}{standard deviation}
   \item{ med}{median}
  \item{qdist}{quartile distance}
 \item{bstats}{boxplot quantiles}
   \item{mstats}{vector of mean and std}
  \item{ N}{number of points}

}

\author{Jonathan M. Lees<jonathan.lees.edu>}

\seealso{boxplot, mean, median}
\examples{

x  = rnorm(100, m=43)
jstats(x)

}

\keyword{misc}
