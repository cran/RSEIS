\name{DECIMATE.SEISN}
\alias{DECIMATE.SEISN}

\title{ Decimate a set of traces }
\description{
  Decimate, or reduce the sample rate of
  a set of traces stored in RSEIS format
}
\usage{
DECIMATE.SEISN(TH, sel=1:length(TH$JSTR), dec=5 ,
 type="LP", proto="BU" , fl=2,  fh=10, RM=FALSE, zp=TRUE )
}
\arguments{

  \item{TH}{RSEIS list }
  \item{sel}{numeric,  which traces to select }
  \item{dec}{numeric, number of samples to skip }
  \item{type}{type of filter (see butfilt)  }
    \item{proto}{filter proto type }
    \item{fl}{low pass frequency cut off }
       \item{fh}{high pass frequency cut off }
        \item{RM}{Remove mean value from trace, default=FALSE}
  \item{zp}{zero phase filter, default=TRUE}
}
\details{
  Reduces the number of samples by skipping every "dec"
  sample.  A filter is applied prior to decimation.
}
\value{
  an RSEIS list.
}

\author{Jonathan M. Lees<jonathan.lees@unc.edu>}
\note{ The dt, n and t2 are modified in info.
}
\seealso{butfilt }
\keyword{misc}
