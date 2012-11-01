\name{sigconv}
\Rdversion{1.1}
\alias{sigconv}
\title{Convolve spikes with wavelets
}
\description{Convolve spikes with wavelets
}
\usage{
sigconv(wigmat, wavepulse)
}
\arguments{
  \item{wigmat}{matrix, spikes
}
  \item{wavepulse}{wavelet for convolution
}
}
\details{Convolution is done in Frequency domain on each trace
}
\value{Matrix, waveforms
}

\author{
Jonathan M. Lees<jonathan.lees@unc.edu>
}


\seealso{wiggleimage, symshot1, genrick
}
\examples{
S1 <- symshot1()

##############   S1$THEORY$treflex

d <- dim(S1$smograms)
G1 <- matrix( rep(0, length=d[1]*d[2]), ncol=d[2], nrow=d[1])

############   set up the spike set for reflexions
for(i in 1:3){
p <- round( S1$THEORY$treflex[i,]/S1$dt );

G1[cbind(p , 1:d[2])  ] <- 1

}

#### plot the spikes
wiggleimage(0.1*G1, dt = -S1$dt, dx = S1$x, col = "black")

###  make a ricker wavelet
wavelet <- genrick(25,S1$dt,35)
klem <- 11
### 
nwave <- RESCALE(wavelet, 0, 1, wavelet[1], max(wavelet))

##############  convolve the wavelet with the set of spikes
H1 <- sigconv(G1, nwave)

############  plot
wiggleimage(0.1*H1, dt = -S1$dt, dx = S1$x, col = "black")


}
\keyword{misc}
