\name{mtapspec}
\alias{mtapspec}

\title{MTM spectrum}
\description{
 Multi-tape Method Spectrum
}
\usage{
mtapspec(a, dt, klen = length(a), MTP = NULL)
}

\arguments{
  \item{a}{vector time series}
  \item{dt}{sample rate}
  \item{klen}{length of fft}
  \item{MTP}{MTM parameters, list:
    \describe{
      \item{kind}{kind of taper average}
      \item{nwin}{number of windows}
      \item{npi}{number of Pi-prolate functions}
      \item{inorm}{normalization flag}
    }
  }
}
\details{
MTP represent parameters that control the multi-tape pi-prolate
functions used by mtapspec.  See reference for details.

}
\value{
  LIST
    \item{dat}{input data}
    \item{dt}{sample rate}
    \item{spec}{Estimated power spectrum}
    \item{dof}{degrees of freedom for each frequency}
    \item{Fv}{F-values for each frequency}
    \item{Rspec}{real part of complex spectrum}
    \item{Ispec}{imaginary part of complex spectrum}
    \item{freq}{frequencies}
    \item{df}{delta frequency}
    \item{numfreqs}{number of frequencies}
    \item{klen}{length used in fft}
    \item{mtm}{input MTM parameters, see above}
}
\references{Lees, J. M. and Park, J., 1995: Multiple-taper spectral analysis: A stand-alone C-subroutine, \emph{Computers \& Geology}, 21(2), 199-236. }
\author{Jonathan M. Lees<jonathan.lees.edu>}

\seealso{fft }
\examples{

data(CE1)
a = list(y=CE1$y[CE1$x>5.443754 & CE1$x<5.615951], dt=CE1$dt)

    Mspec =   mtapspec(a$y,a$dt, klen=4096, MTP=list(kind=2,nwin=5, npi=3,inorm=0)  )   

}

\keyword{misc}
