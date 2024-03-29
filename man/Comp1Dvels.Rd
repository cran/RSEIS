\name{Comp1Dvels}
\alias{Comp1Dvels}
\title{Compare 1D models }
\description{
   Plot 1D velocity Models for comparison.
}
\usage{
Comp1Dvels(INV, depth = 1:50)
}
\arguments{
  \item{INV}{vector of velocity models in memory}
  \item{depth}{depth range for plotting}
}
\details{
  takes several velocity models, finds the range of all,
  makes a plot so that all models fit on figure.
}
\value{
 Graphical Side Effects
}

\author{Jonathan M. Lees<jonathan.lees@unc.edu>}

\seealso{Plot1Dvel, Comp1Dvel, Get1Dvel}
\examples{

VEL <- list()
VEL$'zp' <- c(0,0.25,0.5,0.75,1,2,4,5,10,12)
VEL$'vp' <- c(1.1,2.15,3.2,4.25,5.3,6.25,6.7,6.9,7,7.2)
VEL$'ep' <- c(0,0,0,0,0,0,0,0,0,0)
VEL$'zs' <- c(0,0.25,0.5,0.75,1,2,4,5,10,12)
VEL$'vs' <- c(0.62,1.21,1.8,2.39,2.98,3.51,3.76,3.88,3.93,4.04)
VEL$'es' <- c(0,0,0,0,0,0,0,0,0,0)
VEL$'name' <- '/data/wadati/lees/Site/Hengil/krafla.vel'

VELNish <- list()
VELNish$'zp' <- c(0,0.1,0.6,1.1,21.1)
VELNish$'vp' <- c(2.8,3.4,4.1,4.7,4.7)
VELNish$'ep' <- c(0,0,0,0,0)
VELNish$'zs' <- c(0,0.1,0.6,1.1,21.1)
VELNish$'vs' <- c(1.6,2,2.4,2.7,2.7)
VELNish$'es' <- c(0,0,0,0,0)
VELNish$'name' <- 'Nish'

Comp1Dvels(c("VEL", "VELNish"))




}
\keyword{hplot}
