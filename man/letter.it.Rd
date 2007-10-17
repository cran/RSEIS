\name{letter.it}
\alias{letter.it}

\title{Add letters to the corners of plots in multiple figures}
\description{
  Add letters to the corners of plots in multiple figures
}
\usage{
letter.it(a, corn = 1)
}

\arguments{
  \item{a}{character letter for marking figure}
  \item{corn}{corner to put letter in}
}
\details{
  Can use uppercase or lower case letters, or roman numerals.
}
\value{
Graphical Side Effects
}

\author{Jonathan M. Lees<jonathan.lees.edu>}

\examples{
par(mfrow=c(2,2))
for(i in 1:4)
{
x = 1:10
y = rnorm(10)
plot(x,y)
letter.it(letters[i], 2)

  }
}

\keyword{aplot}
