\name{lagplot}
\alias{lagplot}

\title{Plot phase lags}
\description{
 adds a bar showing phase lag.
}
\usage{
lagplot(y1, dt, lag, PLOT = FALSE)
}

\arguments{
  \item{y1}{seismic signal }
  \item{dt}{DeltaT, s }
  \item{lag}{lag, s }
  \item{PLOT}{logical, TRUE=plot }
}

\value{
 Graphical Side Effects.
}

\author{Jonathan M. Lees<jonathan.lees.edu>}
\seealso{getphaselag2}

\examples{


\dontrun{
plot(MTS1$freq, nphase,  main=paste(sep=' ', "thelag=",phaselag) )
     abline(MOD)
    
 lagplot(ts1, DT, phaselag, PLOT=TRUE )
}



}

\keyword{aplot}
