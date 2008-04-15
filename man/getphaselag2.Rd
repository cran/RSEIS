\name{getphaselag2}
\alias{getphaselag2}

\title{Phase Lag}
\description{
  Use MTM spectrum to estimate phase lag between two signals. 
}
\usage{
getphaselag2(y1, y2, DT = 0.008, frange = c(0, 20), PLOT = FALSE, PLOT1 = FALSE, PLOT2 = FALSE)
}

\arguments{
  \item{y1}{vector times series one}
  \item{y2}{vector times series two }
  \item{DT}{deltaT sample rate, s }
  \item{frange}{vector, frequency bounds for analysis}
  \item{PLOT}{logical, TRUE=diagnostic plot}
  \item{PLOT1}{logical, TRUE=diagnostic plot}
  \item{PLOT2}{logical, TRUE=diagnostic plot}
}
\details{
  uses the slope of the cross spectrum to estimate the phase lag.
  
}
\value{
 phase lag, seconds
}

\author{Jonathan M. Lees<jonathan.lees.edu>}

\seealso{mtapspec}
\examples{
data("GH")

Xamp1=GH$JSTR[[1]]
Xamp1=Xamp1[1123:2000]

Xamp2= GH$JSTR[[4]]
Xamp2=Xamp2[1123:2000]
plot(Xamp1,type='l')
lines(Xamp2,type='l',col='red')

 pshift = getphaselag2(Xamp1, Xamp2,  DT=GH$info$dt[1],  frange=c(5, 15),  PLOT=TRUE)

}

\keyword{misc}
