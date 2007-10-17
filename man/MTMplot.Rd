\name{MTMplot}
\alias{MTMplot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Plot Multi-taper Spectrum}
\description{
  Plots output of MTM specturm
}
\usage{
MTMplot(a, f1 = f1, f2 = f2, len2 = 1024, PLOT = FALSE, PADDLAB = NULL, GUI = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{a}{signal}
  \item{f1}{lower frequency}
  \item{f2}{upper frequency}
  \item{len2}{number of points in spectrum}
  \item{PLOT}{logical, TRUE=plot}
  \item{PADDLAB}{Labels for buttons}
  \item{GUI}{use a GUI to display for other interactions}
}
\details{
 Uses Lees' MTM code.
}
\value{

   list(len2=len2, f=f, f1=f1, f2=f2, displ=displ, ampsp=amp, flag=flag )

  \item{len2}{next power of 2 for fft calculation}
  \item{f}{frequencies}
  \item{f1 }{lower freq}
  \item{f2}{upper freq}
  \item{displ}{kind of display}
  \item{ampsp}{amplitude spectrum}
  \item{flag}{}
  
}
\references{}
\author{Jonathan M. Lees<jonathan.lees.edu>}

\seealso{drive.MTM, MTMdisp, plt.MTM0}
\examples{

}

\keyword{misc}
\keyword{hplot}