\name{PLTpicks}
\alias{PLTpicks}

\title{Plot picks on seismic record}
\description{
  Add lines at phase arrival times
}
\usage{
PLTpicks(picks, labs = NA, cols = NA)
}

\arguments{
  \item{picks}{vector of times relative to the start of the plot}
  \item{labs}{labels for picks }
  \item{cols}{colors for picks }
}
\details{
   picks = vector of times relative to the start of the plot (seismogram)
}
\value{
 Graphical Side Effects
}
\examples{

ex = seq(from=0, to=4*pi, length = 200)

y = sin(ex)
picks = c(0.5*pi, 2.3*pi)
plot(ex, y, type='l')


PLTpicks(picks, labs =c("P","P")  , cols =c('red','green')  )

PLTpicks(picks+2, labs =c("S","PKIKP") , cols ='blue'  )

}




\author{Jonathan M. Lees<jonathan.lees.edu>}

\keyword{aplot}
