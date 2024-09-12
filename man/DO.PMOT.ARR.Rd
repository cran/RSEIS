\name{DO.PMOT.ARR}
\alias{DO.PMOT.ARR}

\title{Particle Motion Analysis with arrows}
\description{
 Plot particle motion arrows
}
\usage{
DO.PMOT.ARR(E, N)
}

\arguments{
  \item{E}{East component}
  \item{N}{East Component}
}

\value{
Graphical Side Effects
}

\author{Jonathan M. Lees<jonathan.lees.edu>}

\seealso{PMOT.drive}
\examples{

data(GH)
XLIM = c(1226, 1322 )

e = GH$JSTR[[1]][XLIM[1]:XLIM[2]]
n = GH$JSTR[[2]][XLIM[1]:XLIM[2]]


   xx = range(e, na.rm =TRUE)
    yy = range(n, na.rm =TRUE)
 sx = range(c(xx, yy))

   x  = RPMG::RESCALE(e, 0, 1, sx[1], sx[2])
    y  = RPMG::RESCALE(n, 0, 1, sx[1], sx[2])



plot(range(x), range(y) , type='n')
lines(x, y, col=grey(0.8) )
DO.PMOT.ARR(x, y)


}

\keyword{misc}
\keyword{aplot}
