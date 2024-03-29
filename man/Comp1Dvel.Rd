\name{Comp1Dvel}
\alias{Comp1Dvel}

\title{Compare a pair of 1D models }
\description{
 plot a pair of 1D velocity Models for comparison
}
\usage{
Comp1Dvel(v, v2, col=c('blue', 'brown'), ...)
}

\arguments{
  \item{v}{List structure for model 1}
  \item{v2}{List structure for model 2}
   \item{col}{2-colors for P and swave}
   \item{\dots}{other graphical parameters (e.g. lty, lwd) }
}

\value{
 Graphical Side Effects
}

\author{Jonathan M. Lees<jonathan.lees@unc.edu>}

\seealso{Plot1Dvel, Get1Dvel, travel.time1D}
\examples{
VEL <- list()
VEL$'zp' <- c(0,0.25,0.5,0.75,1,2,4,5,10,12)
VEL$'vp' <- c(1.1,2.15,3.2,4.25,5.3,6.25,6.7,6.9,7,7.2)
VEL$'ep' <- c(0,0,0,0,0,0,0,0,0,0)
VEL$'zs' <- c(0,0.25,0.5,0.75,1,2,4,5,10,12)
VEL$'vs' <- c(0.62,1.21,1.8,2.39,2.98,3.51,3.76,3.88,3.93,4.04)
VEL$'es' <- c(0,0,0,0,0,0,0,0,0,0)
VEL$'name'  <-  '/data/wadati/lees/Site/Hengil/krafla.vel'

VELNish <-  list()
VELNish$'zp' <- c(0,0.1,0.6,1.1,21.1)
VELNish$'vp' <- c(2.8,3.4,4.1,4.7,4.7)
VELNish$'ep' <- c(0,0,0,0,0)
VELNish$'zs' <- c(0,0.1,0.6,1.1,21.1)
VELNish$'vs' <- c(1.6,2,2.4,2.7,2.7)
VELNish$'es' <- c(0,0,0,0,0)
VELNish$'name' <- 'Nish'

Comp1Dvel(VEL, VELNish)

}

\keyword{hplot}
