\name{one}
\alias{one}

\title{one plotting region}
\description{
  change from multiple R-screens to one
}
\usage{
one()
}
\examples{

par(mfrow=c(2,1))

plot(rnorm(10), rnorm(10) )

plot(rnorm(10), rnorm(10) )

one()
plot(rnorm(10), rnorm(10) )


}



\keyword{misc}