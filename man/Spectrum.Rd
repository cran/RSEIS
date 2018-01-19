\name{Spectrum}
\alias{Spectrum}
\title{Calculate Different Spectrum Types in Physical Units}
\description{Spectrum is a wrapper function for stats::fft and
  RSEIS::mtapspec. For a given method (multi-taper spectrum or fft
  spectrum) and spectrum type (power, energy, amplitude, or phase), it
  returns the spectrum in physical units (obeying Parseval's theorem)
  and the corresponding frequency axis.}
\usage{
Spectrum(x, dt, one_sided = TRUE, type = 1, method = 1)
}
\arguments{
  \item{x}{Time series for which a spectrum is to be calculated (assumed
  to be in volts)}
  \item{dt}{Sample interval for x (assumed to be in seconds)}
  \item{one_sided}{Logical: should the spectrum be a function of
    positive frequencies only (f < nyquist frequency) and spectral
    density doubled to be consistent with that (TRUE, default), or
    should the spectrum be provided for all frequencies, positive and
    negative?}
  \item{type}{Type of spectrum: 1 (default) is power spectrum; 2 is
    energy spectrum; 3 is amplitude spectrum; 4 is phase spectrum}
  \item{method}{Method used to calculate spectrum. 1 (default) is fft; 2
  is multi-taper.}
}
\details{
  Phase spectrum is currently enabled only for method = 1 (fft). All
  possible energy and power spectra obey Parseval's relation (sum(s)*df
  ~= mean(x^2) for power; sum(s)*df ~= sum(x^2)*dt for
  energy). Parseval's relation may not be exact due to approximations
  used in making the spectrum one-sided or in the multi-taper method.

  Input units are assumed to be volts and seconds; if other input units
  are used, adjust output units accordingly.
}
\value{
  List with following elements.
  \item{f}{frequency axis (Hz; cycles per second, not radians per
    second)}
  \item{df}{interval for frequency axis (Hz)}
  \item{spectrum}{spectral values corresponding to f}
  \item{type}{spectrum type: Power, Energy, Amplitude, or Phase}
  \item{units}{Units of spectrum (assuming that input units are volts
  and seconds)}
}
\author{
Jake Anderson
}

\seealso{
  RSEIS::mtapspec
  stats::fft
}
\examples{
## example time series
x = rnorm(1000)
dt = 0.01

## power spectrum, multi-taper method, one-sided
S = Spectrum(x, dt, type = 1, method = 2, one_sided = TRUE)
sum(S$spectrum) * S$df ## frequency-domain power
mean(x^2) ## time-domain power

## energy spectrum, fft method, two-sided
S = Spectrum(x, dt, type = 2, method = 1, one_sided = FALSE)
sum(S$spectrum) * S$df ## frequency-domain energy
sum(x^2) * dt ## time-domain energy
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ math }% use one of  RShowDoc("KEYWORDS")

