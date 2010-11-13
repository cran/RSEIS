\name{comp.env}
\alias{comp.env}

\title{Compare Envelopes}
\description{
 calculate and plot signal envelopes.
}
\usage{
comp.env(ex, Y, PLOT = TRUE, stamps = stamps)
}

\arguments{
  \item{ex}{x-axis}
  \item{Y}{matrix of Y values}
  \item{PLOT}{logical, TRUE=plot}
  \item{stamps}{character vectors of ids}
}
\details{
Takes in an common x predictor and
  compares the envelopes of each column in the Y matrix.
   All the Y's must have the same length as ex.
 
}
\value{
  Graphical Side effects.
  returns envelope series.
}

\author{Jonathan M. Lees<jonathan.lees.edu>}

\seealso{swig}
\examples{
data("GH")

temp = cbind(GH$JSTR[[1]], GH$JSTR[[2]], GH$JSTR[[3]])

atemp =temp[1168:1500, ]
ex  = seq(from=0,length=length(temp[1168:1500, 1]), by=GH$dt[1])

comp.env(ex, atemp, PLOT = TRUE, stamps = c("1","2", "3") )
}

\keyword{aplot}
