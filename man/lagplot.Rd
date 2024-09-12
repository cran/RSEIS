\name{lagplot}
\alias{lagplot}

\title{Plot phase lags}
\description{
 Shift a times series by a specified  phase lag.
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

data(KH)

ts1 = KH$JSTR[[1]]

 lagplot(ts1, KH$dt[1], 300, PLOT=TRUE )


}

\keyword{aplot}
