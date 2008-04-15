\name{wlet.do}
\alias{wlet.do}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Return Wavelet transform}
\description{
  Wavelet transform
}
\usage{
wlet.do(why, dt, noctave = 6, nvoice = 20, flip = TRUE, ploty = TRUE, zscale = 1, col = terrain.colors(100), STAMP = STAMP)
}

\arguments{
  \item{why}{signal}
  \item{dt}{sample rate (s)}
  \item{noctave}{number of octives}
  \item{nvoice}{number of voices }
  \item{flip}{logical,  whether to flip the orientation }
  \item{ploty}{logical, whether to plot y }
  \item{zscale}{scale of the image }
  \item{col}{color palette}
  \item{STAMP}{cahracter stamp for identification}
}
\details{
 IUses the ccw code to calculate the transform, but plots differently.
 Morelet wavelet is used by default.
}
\value{
  \item{baha}{wavelet transform list}
    \item{PE}{plotting information list}
}

\author{Jonathan M. Lees<jonathan.lees.edu>}

\seealso{Rwave}
\examples{

data(CE1)

plot(CE1$x, CE1$y, type='l')

require(Rwave)

wlet.do(CE1$y, CE1$dt, flip = FALSE, ploty = TRUE)



}

\keyword{misc}
\keyword{hplot}