\name{ETECTG}
\alias{ETECTG}
\title{Event Detection}
\description{
  Event Detection for a seismic section
}
\usage{
ETECTG(GH, sel = sel, FRWD = 8, BKWD = 8, sbef = 1, saft = 6, DFRWD = 0.5, DBKWD = 0.5, thresh = 2, Tthresh2 = 7, stretch = 1000, flo = 0.1, fhi = 5, PLOT = FALSE, Kmin = 7, perc = 0.05, kind = 1, DOARAIC = FALSE)
}

\arguments{
  \item{GH}{Seismic Structure}
  \item{sel}{select traces}
  \item{FRWD}{forward window, s}
  \item{BKWD}{backward window }
  \item{sbef}{seconds before}
  \item{saft}{seconds after}
  \item{DFRWD}{seconds before}
  \item{DBKWD}{seconds after}
  \item{thresh}{threshold 1}
  \item{Tthresh2}{threshold 2}
  \item{stretch}{stretch factor}
  \item{flo}{low frequency for BP filter}
  \item{fhi}{low frequency for BP  filter}
  \item{PLOT}{logical, TRUE=plot diagnostics}
  \item{Kmin}{min number of picks per window}
  \item{perc}{percentage of Kmin allowed}
  \item{kind}{kind of picking}
  \item{DOARAIC}{TRUE=do auto-regressive AIC method }
}
\details{
  Very complicated picking routine - designed for volcanic
  regions with emergent arrivals.  Works with lots of tuning.
}
\value{
  \item{sel}{input selection}
 \item{JJ}{index}
 \item{PPTIM}{p-arrivals}
 \item{PP}{all arrivals}
  
 
}

\author{Jonathan M. Lees<jonathan.lees.edu>}

\keyword{misc}
