\name{genrick}
\Rdversion{1.1}
\alias{genrick}
\title{Ricker Wavelet
}
\description{Generate a ricker wavelet of a specfied
  frequency and length
}
\usage{
genrick(freq, dt, nw)
}
\arguments{
  \item{freq}{ frequency of ricker wavelet
}
  \item{dt}{ Time sample rate (s)
}
  \item{nw}{ length of wavelet.
}
}

\value{ricker wavelet as a vector.
}

\author{
Jonathan M. Lees<jonathan.lees@unc.edu>
}
\note{
Original code by
 Leonard Lisapaly (leonardl@fisika.ui.ac.id), converted to R by J.M. Lees.
  
}


\examples{

dt <- 0.01
freq <- 16
nlen <- 35

G <- genrick(freq, dt, nlen)

tee <- seq(from=0, by=dt, length=length(G))

plot(tee, G, type='l')

}
\keyword{misc}
