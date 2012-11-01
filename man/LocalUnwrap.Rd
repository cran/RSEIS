\name{LocalUnwrap}
\alias{LocalUnwrap}

\title{Unwrap spectrum phase}
\description{
 unwrap the phase spectrum so it does not wrap around 
}
\usage{
LocalUnwrap(p, cutoff = cutoff)
}

\arguments{
  \item{p}{phase spectrum}
  \item{cutoff}{cut off angle = pi }
}

\value{
  Unwrapped spectrum
}
\examples{

x  <-  1:512
amp <- sin(1*2*pi*x/16) + sin(2*2*pi*x/16)  + sin(3*2*pi*x/16)


spc <- fft(amp)

plot(Mod(spc), type='l')

angle  <-  Arg(spc)

plot(angle, type='l')

unang  <-  LocalUnwrap(angle, cutoff =pi )
plot(unang, type='l')

}



\author{Jonathan M. Lees<jonathan.lees.edu>}
\note{ 
  Algorithm minimizes the incremental phase variation by
  constraining it to the range [-pi,pi]
 
}


\keyword{misc}
