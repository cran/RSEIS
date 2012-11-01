\name{setwelch}
\alias{setwelch}
\title{Set up Matrix of fft for Welch method}
\description{
Prepares a matrix for estimation of power spectrum via Welch's method.
  Also, is can be used for spectrogram.
}
\usage{
setwelch(X, win = min(80, floor(length(X)/10)),
inc = min(24, floor(length(X)/30)), coef = 64, wintaper=0.05)
}
\arguments{
  \item{X}{Time series vector}
  \item{win}{window length}
  \item{inc}{increment}
  \item{coef}{coefficient for fft}
  \item{wintaper}{percent taper window taper}
}

\value{
  List:
\item{values}{Matrix of fft's staggered along the trace}
\item{windowsize}{window length used}
\item{increment}{increment used}
\item{wintaper}{percent taper window taper}


}
\references{
Welch, P.D. (1967) The use of Fast Fourier Transform for the estimation of power spectra:
a method based on time averaging over short, modified periodograms IEEE Trans. Audio
Electroacoustics 15, 70-73. 

}
\author{originally written by Andreas Weingessel, modified Jonathan M. Lees<jonathan.lees@unc.edu>}

\seealso{stft}
\examples{

  
dt <- 0.001

 t <- seq(0, 6, by=dt)
x <- 6*sin(2*pi*50*t) + 10* sin(2*pi*120*t)
 y <- x + rnorm(length(x), mean=0, sd=10)

plot(t,y, type='l')

title('sin(2*pi*50*t) + sin(2*pi*120*t)+ rnorm')

Y <- fft(y)

Pyy <- Y * Conj(Y)

N <- length(y)

n <- length(Pyy)/2

Syy <- (Mod(Pyy[1:n])^2)/N

fn <- 1/(2*dt)


f <- (0:(length(Syy)-1))*fn/length(Syy)

plot(f, Syy, type='l', log='y' , xlim=c(0, 150));
abline(v=c(50, 120),col='blue', lty=2)


plot(f, Syy, type='l', log='y' , xlim=c(0, 150));
abline(v=c(50, 120),col='blue', lty=2)


win <- 1024

inc <- min(24, floor(length(y)/30))
coef <- 2048


 w <- setwelch(y, win=win, inc=inc, coef=coef, wintaper=0.2)

    KK <- apply(w$values, 2, FUN="mean")


fw <- seq(from=0, to=0.5, length=coef)/(dt)

plot(fw, KK^2, log='', type='l' , xlim=c(0, 150)) ;
abline(v=c(50, 120), col='blue', lty=2)


Wyy <- (KK^2)/w$windowsize
plot(f, Syy, type='l', log='y' , xlim=c(0, 150))
lines(fw,Wyy , col='red')


DBSYY <- 20*log10(Syy/max(Syy))
DBKK <- 20*log10(Wyy/max(Wyy))


plot(f, DBSYY, type='l' , xlim=c(0, 150), ylab="Db", xlab="Hz")

lines(fw, DBKK, col='red')
title("Compare simple periodogam with Welch's Method")



}
\keyword{misc}
