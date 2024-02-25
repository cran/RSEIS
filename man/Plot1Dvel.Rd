\name{Plot1Dvel}
\alias{Plot1Dvel}
\title{Plot 1D Velocity Model
}
\description{Plot 1D velocity model
  showing P-wave and S-wave layered models.
}
\usage{
Plot1Dvel(v, tit = NULL, col=c('blue', 'brown'), ...)
}
\arguments{
  \item{v}{Velocity models 
}
  \item{tit}{Title for plot (character)
  }
  \item{col}{2-colors for P and swave
  }
\item{\dots}{other graphical parameters (e.g. lty, lwd) }


  
}
\details{
  Velocity model consists of a list
  of P and S depths and layer velocity values.
  See example below.
}
\value{Graphical Side effect
}

\author{
Jonathan M. Lees<jonathan.lees@unc.edu>
}
\note{ Errors are not required, although future
  versions may include the plotting of error bars.
}


\seealso{Get1Dvel, Comp1Dvel, Comp1Dvels, travel.time1D}

\examples{


VEL <- list()
VEL$'zp' <- c(0,0.25,0.5,0.75,1,2,4,5,10,12)
VEL$'vp' <- c(1.1,2.15,3.2,4.25,5.3,6.25,6.7,6.9,7,7.2)
VEL$'ep' <- c(0,0,0,0,0,0,0,0,0,0)
VEL$'zs' <- c(0,0.25,0.5,0.75,1,2,4,5,10,12)
VEL$'vs' <- c(0.62,1.21,1.8,2.39,2.98,3.51,3.76,3.88,3.93,4.04)
VEL$'es' <- c(0,0,0,0,0,0,0,0,0,0)
VEL$'name' <- '/data/wadati/lees/Site/Hengil/krafla.vel'

Plot1Dvel(VEL, tit = 'This is an Example' )

}
\keyword{misc}
