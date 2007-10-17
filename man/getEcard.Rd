\name{getEcard}
\alias{getEcard}

\title{Error Card}
\description{
 Location Error Card
}
\usage{
getEcard(ECARD)
}

\arguments{
  \item{ECARD}{error card from Lquake }
}

\value{
  
  \item{LOC}{character, location }
  \item{rms}{root mean square error}
  \item{meanres}{mean residual}
  \item{sdres}{standard deviation of residuals}
  \item{sdmean}{standard error of mean}
  \item{sswres}{sum squares}
  \item{ndf}{number degrees of freedom}
  \item{fixflgs }{flags for inversion}
  \item{sterrx }{error in x-direction}
  \item{sterry}{error in y-direction}
  \item{sterrz}{error in z-direction}
  \item{sterrt}{error in origin time}
  \item{mag}{mag}
  \item{sterrmag}{error for mag}

}

\author{Jonathan M. Lees<jonathan.lees.edu>}

\keyword{misc}