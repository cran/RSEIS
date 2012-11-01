\name{finteg}
\alias{finteg}

\title{Integration in Frequency Domain}
\description{
  Integration of seismic signal in Frequency Domain.
Used for converting velocity seismogram to displacement.
  
}
\usage{
finteg(data, dt)
}

\arguments{
  \item{data}{time series}
  \item{dt}{sample interval}
}

\value{
  Integrated time series signal
}
\note{
  To avoid problems with dividing by very small numbers,
  water level is set =1.e-8
}



\author{Jonathan M. Lees<jonathan.lees.edu>}

\examples{

   ##  waterlevel=1.e-8


    dfor5 <- rnorm(1000)

  idfor5 <- finteg(dfor5, 0.008)
 
}

\keyword{misc}
