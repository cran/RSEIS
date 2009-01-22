\name{FILT.spread}
\alias{FILT.spread}

\title{Filter trace with a spread of filters}
\description{
  Show a time series and a spread of user defined filters to show signal
  at a variety of bandwidths.
}
\usage{
FILT.spread(x, y, dt, fl = fl, fh = fh, sfact = 1, WIN = NULL, PLOT = TRUE, TIT = NULL, TAPER = 0.05, POSTTAPER=0.05)
}

\arguments{
  \item{x}{x-axis}
  \item{y}{y-amplitude}
  \item{dt}{delta-t, sec}
  \item{fl}{vector of low frequency cut offs}
  \item{fh}{vector of high frequency cut offs}
  \item{sfact}{scale factor, 0,1 }
  \item{WIN}{xlimits to constrain plotting}
  \item{PLOT}{logical, plotting}
  \item{TIT}{title}
  \item{TAPER}{taper data prior to filter, percent cosine, default=NULL}
  \item{POSTTAPER}{taper output after filter, percent cosine, default=0.05}
    
}
\details{
  Use the TAPER and POSTTAPER to reduce the edge effects
  prior to and after filtering.
  
}
\value{
 list:
  \item{FMAT}{matrix of time series filtered}
  \item{Notes}{Notes for filter of each element of FMAT}
 
}

\author{Jonathan M. Lees<jonathan.lees@unc.edu>}

\seealso{butfilt, PLOT.MATN}
\examples{
data(KH)
dt = KH$dt[1]

y =  KH$JSTR[[1]]

x =  seq(from=0, by=dt, length=length(y))



fl=rep(1/100, 5)
fh=1/c(1,2,5,10,20)

FILT.spread(x, y, dt, fl = fl, fh = fh, sfact = 1, WIN = NULL, PLOT = TRUE, TIT = NULL, TAPER = 0.05)


}

\keyword{hplot}
