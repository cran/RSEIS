\name{peaks}
\alias{peaks}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Peaks}
\description{
  Find peak amplitudes in a time series signal.
}
\usage{
peaks(series, span = 3, do.pad = TRUE)
}
%- maybe also 'usage' for other objects documented here.
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
\references{ }
\author{Brian Ripley}

\examples{

data(CE1)
plot(CE1$x, CE1$y, type='l')


pp = seq(from=53, to=80, by=1)

plot(CE1$x[pp], CE1$y[pp], type='l')


aa = peaks(CE1$y[pp], span=3)


abline(v=CE1$x[pp[aa]], col='red')



}

\keyword{misc}
