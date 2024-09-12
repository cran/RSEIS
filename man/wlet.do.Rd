\name{wlet.do}
\alias{wlet.do}

\title{Return Wavelet transform}
\description{
  Wavelet transform
}
\usage{
wlet.do(why, dt, noctave = 6, nvoice = 20, w0=5,
flip = TRUE, ploty = TRUE, zscale = 1,
col = terrain.colors(100), STAMP = STAMP, units="", scaleloc=c(0.4,0.95))
}

\arguments{
  \item{why}{signal}
  \item{dt}{sample rate (s)}
  \item{noctave}{number of octaves, default=6}
  \item{nvoice}{number of voices, nvoice = 20 }
  \item{w0}{central frequency for morlet wavelet, default=5}
  \item{flip}{logical,  whether to flip the orientation }
  \item{ploty}{logical, whether to plot y }
  \item{zscale}{scale of the image }
  \item{col}{color palette}
  \item{STAMP}{cahracter stamp for identification}
  \item{units}{character, units to put on plot}
    \item{scaleloc}{2-vector, percentatge of bottom margin for the color
  scale}
}
\details{
  This function uses the cwt (package:Rwave) code to calculate the continuous wavelet
  transform,
  but plots it differently.
 Morelet wavelet is used by default.   The cwt produces an
 image, the modulus of the transform, which is passed on to wlet.do
 along with the number of octaves and the number of voices.
 Plotting parameters are passed to the function so that replotting can
 be accomplished (use plotwlet) without having to recalculate the
 transform.

 Plotting parameters are passed on to the plotting function, plotwlet.
 
 
}
\value{
  \item{baha}{list:
    wavelet transform image,
    noctave = number of  octaves,
    nvoice = number of voices, w0= central freq, 
        flip = logical, whether image is flipped (default=TRUE) }
      \item{PE}{plotting information list:
	why=y-axis,
	dt=time series sample,
	interval,
	zscale=(1,2,3) image scaling,
	col=color map,
        ygrid = logical(default=FALSE),
	STAMP = character string }
}

\author{Jonathan M. Lees<jonathan.lees.edu>}

\seealso{Rwave,  cwt , plotwlet, contwlet , pwlet2freqs, wlet.drive}
\examples{

data(CE1)

plot(CE1$x, CE1$y, type='l')

require(Rwave)

out <- wlet.do(CE1$y, CE1$dt, flip = FALSE, ploty = TRUE)



}

\keyword{misc}
\keyword{hplot}