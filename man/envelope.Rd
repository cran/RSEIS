\name{envelope}
\alias{envelope}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Envelope Function with Hilbert Transform}
\description{
  Envelope Function with Hilbert Transform
 
}
\usage{
envelope(x)
}

\arguments{
  \item{x}{signal vector}
}
\details{
 Uses the hilbert transform to get the envelope function.
}
\value{
 
 vector of the absolute of the hilbert transform
}

\author{Jonathan M. Lees<jonathan.lees.edu>}


\examples{

data(CE1)
ev = envelope(CE1$y)
plot(CE1$x, CE1$y, type='l')
lines(CE1$x,ev, col='red')


}

\keyword{misc}
