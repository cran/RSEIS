\name{INSTresponse}
\alias{INSTresponse}

\title{Instrument Respnse Function}
\description{
 Extract Instrument Response from Poles and Zeros
}
\usage{
INSTresponse(Kal, key, ff, tt = tt, plotkey = NULL)
}

\arguments{
  \item{Kal}{Calibration}
  \item{key}{index to list of instruments}
  \item{ff}{frequency vector}
  \item{tt}{time vector}
  \item{plotkey}{TRUE = plot}
}
\details{
  response is fourier transform of delta function run through the filter
}
\value{
  List:
  \item{transfer}{transfer function}
  \item{aa}{a coeficients}
  \item{bb}{b coeficients}
  \item{resp}{real part of response}
  
}
\references{Adapted from Ken Creager's Matseis}
\author{Jonathan M. Lees<jonathan.lees.edu>}

\seealso{deconinst}
\examples{


########  set list of possible instruments:
Kal  <-  PreSet.Instr()
###  get instrument reponse for first in list:
resp1   <-  INSTresponse(Kal, 1, c(0,100)  , tt=c(1,0.008), plotkey=TRUE)
###  plots amplitude and phase
}

\keyword{misc}
\keyword{hplot}
