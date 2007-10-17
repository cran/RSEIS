\name{INVRft}
\alias{INVRft}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Inverse Fourier Transform}
\description{
  Inverse Fourier Transform
}
\usage{
INVRft(G, n, tstart, dt)
}

\arguments{
  \item{G}{Input fourier transform}
  \item{n}{length of time vector }
  \item{tstart}{time series starts at tstart}
  \item{dt}{Delta t, sample rate}
}
\details{
  G is a vector spectrum evaluated at positive and negative
  frequencies as defined by makefreq.
  tstart, dt and n define the output time vector as described above.
  
g is the Inverse Fourier Transform of G scaled by dt.
time shift theorem has been used to account for time not starting at t=0.
  
}
\value{

  \item{g}{truncate time vector to N points}
  \item{f}{frequencies}
  \item{t}{times}
 
}

\author{Jonathan M. Lees<jonathan.lees.edu>}

\seealso{makefreq, FRWDft, INSTresponse}
\examples{
zil = rnorm(300)
fss = FRWDft( zil, length(zil), 0, 0.004)   
INVRft(fss$G, length(zil), 0, 0.004)


}

\keyword{misc}
