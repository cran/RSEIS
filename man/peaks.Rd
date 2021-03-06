\name{peaks}
\alias{peaks}

\title{Peaks}
\description{
  Find peak amplitudes in a time series signal.
}
\usage{
peaks(series, span = 3, do.pad = TRUE)
}

\arguments{
  \item{series}{signal}
  \item{span}{span for window}
  \item{do.pad}{padding}
}
\details{
 This function originated in a note from Brian Ripley.
}
\value{
 vector of peak indexes
}

\author{Brian Ripley}

\examples{

data(CE1)
plot(CE1$x, CE1$y, type='l')


pp <- seq(from=53, to=80, by=1)

plot(CE1$x[pp], CE1$y[pp], type='l')


aa <- peaks(CE1$y[pp], span=3)


abline(v=CE1$x[pp[aa]], col='red')



}

\keyword{misc}
