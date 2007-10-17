\name{jlegend}
\alias{jlegend}

\title{plot a legend}
\description{
 Add legend to side of figure
}

\details{
 Rewrite of the legend function for easier manipulation.
}
\value{
See legend() for details on input
  

  
}

\author{Jonathan M. Lees<jonathan.lees.edu>}

\seealso{legend}
\examples{

###  VELMOD1D = Get1Dvel("/home/lees/Progs/R_PAX/wu_coso.vel", PLOT = TRUE)

plot(c(0,1), c(0,1))
u = par('usr')
LEG = jlegend( u[1], u[4], c("Vp", "Vs"), lwd=2, col=c(4,3), plot=FALSE  )

 
}

\keyword{aplot}
