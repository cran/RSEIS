\name{plotwlet}
\alias{plotwlet}

\title{Plot Wavelet Transform}
\description{
 Plot Wavelet Transform
}
\usage{
plotwlet(baha, Ysig, dt, zscale = 1, zbound = NULL,
col = rainbow(100), ygrid = FALSE,
STAMP = "", xlab="Time, s" , units="", scaleloc=c(0.4,0.95))
}

\arguments{
  \item{baha}{Output of wlet.do}
  \item{Ysig}{signal processed}
  \item{dt}{sample rate}
  \item{zscale}{scale of image}
  \item{zbound}{limits on scale}
  \item{col}{color palette}
  \item{ygrid}{add grid}
  \item{STAMP}{character string for identification}
 \item{xlab}{character, label for the x-axis}
 \item{units}{character, units on signal}
 \item{scaleloc}{2-vector, percentatge of bottom margin for the color
  scale} 
}
\details{
This function plots the wavelet transform in a way that is similar to
the spectogram plots.
}
\value{
  list( y=, why=why, yBounds=c(0,perc), x=x, yat=raxspec)
  \item{y}{input signal}
  \item{why}{scaled image}
   \item{yBounds}{vector of boundaries}
   \item{x}{x axis}
  \item{yat}{y axis tic marks}
 
  
Graphical side effects.
}

\author{Jonathan M. Lees<jonathan.lees.edu>}
\seealso{cwt, pwlet2freqs, wlet.do, wlet.drive}


\keyword{hplot}