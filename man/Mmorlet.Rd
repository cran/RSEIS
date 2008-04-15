\name{Mmorlet}
\alias{Mmorlet}

\title{Morlet Wavelet}
\description{
Make Morlet Wavelet
}
\usage{
Mmorlet(UB = -4, LB = 4, N = 256, plot = FALSE)
}

\arguments{
  \item{UB}{upper bound }
  \item{LB}{lower bound}
  \item{N}{number of points}
  \item{plot}{logical, TRUE=plot }
}
\details{
   create a morlet function based on the matlab style routines
}
\value{
  time series list:
  
\item{xval}{x-output}
 \item{morl}{y-output}
  
}

\author{Jonathan M. Lees<jonathan.lees.edu>}

\seealso{scal2freqs, Rwave }
\examples{

mm  = Mmorlet(-8, 8, 256)



}

\keyword{misc}
