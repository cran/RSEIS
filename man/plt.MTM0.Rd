\name{plt.MTM0}
\alias{plt.MTM0}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Plot MTM structure}
\description{
 Plot MTM structure
}
\usage{
plt.MTM0(frange, prange, plxy, M, freqs, amp, a, dof = dof, Fv = Fv, COL = 2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{frange}{frequency range}
  \item{prange}{point range }
  \item{plxy}{log x,y axes }
  \item{M}{structure from MTM}
  \item{freqs}{frequencies }
  \item{amp}{ amplitude }
  \item{a}{}
  \item{dof}{degrees of freedom }
  \item{Fv}{F-values }
  \item{COL}{ color }
}

\value{
 Graphical Side Effect
}
\references{Lees, J. M. and Park, J., 1995: Multiple-taper spectral analysis: A stand-alone C-subroutine, {\emph Computers & Geology}, 21(2), 199-236.}
\author{Jonathan M. Lees<jonathan.lees.edu>}

\seealso{drive.MTM}
\examples{

data(CE1)
plot(CE1$x, CE1$y, type='l')

len = length(CE1$y)
 len2 = 2*next2(len)
 Mspec =   mtapspec(CE1$y,  CE1$dt, klen=len2, MTP=list(kind=1,nwin=5,
npi=3,inorm=0)  )


f=Mspec$freq
M = 1
 f1 = 0.01
f2 = 100
 plxy = ''
 flag = f>=f1 & f <= f2;
    freqs = f[flag]
 mydof = NULL
      myFv = NULL
 amp = Mspec$spec[1:length(f)]

      amp = list(amp[flag])


a = list(y=CE1$y, dt=CE1$dt)
frange = range(freqs, na.rm = TRUE)
 prange = range(amp  , na.rm = TRUE)

###  plot(freqs, amp[[1]])


 plt.MTM0(frange, prange, plxy, M, freqs, amp, a, dof=mydof, Fv=myFv, COL=4)



}

\keyword{hplot}
