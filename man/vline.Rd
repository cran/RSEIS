\name{vline}
\alias{vline}

\title{vertical line on trace panel}
\description{
 add vertical line on trace panel
}
\usage{
vline(x, per = 1, COL = 1, NUM = FALSE, LAB = 1:length(x), lwd = 0, lty = 1)
}

\arguments{
  \item{x}{vector of x-locations}
  \item{per}{percent of window}
  \item{COL}{color}
  \item{NUM}{number lines }
  \item{LAB}{character labels }
  \item{lwd}{line width }
  \item{lty}{line type}
}
\details{
adds vertical lines to plot
}
\value{
 Graphical side Effects
}

\author{Jonathan M. Lees<jonathan.lees.edu>}

\seealso{plocator}
\examples{
plot(c(0,1), c(0,1), type='n')

vline(runif(4),  COL ='red')


}

\keyword{aplot}
