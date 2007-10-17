\name{gaddtix}
\alias{gaddtix}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{add tic marks}
\description{
Add tic marks to plot
}
\usage{
gaddtix(side = 3, pos = 0, tck = 0.005, at = c(0, 1), labels = NULL, col = 2, addline = FALSE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{side}{side = 1, 2, 3, 4}
  \item{pos}{relative to axis}
  \item{tck}{tic length}
  \item{at}{vector of positions}
  \item{labels}{vector of labels}
  \item{col}{color for plotting }
  \item{addline}{add lines}
  \item{\dots}{graphical parameters from par }
}

\value{
  Graphical side effect
}

\author{Jonathan M. Lees<jonathan.lees.edu>}

\seealso{par }
\examples{


plot(c(0,1), c(0,1), type='n', ann=FALSE, axes=FALSE)

gaddtix(side=1, pos=0,   tck=-0.01, at=seq(from=0, to=.5, by=.2) , labels=seq(from=0, to=.5, by=.2), col=1)

}

\keyword{misc}
\keyword{aplot}