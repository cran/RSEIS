\name{DECIMATE.SEISN}
\alias{DECIMATE.SEISN}

\title{ Decimate a set of traces }
\description{
  Decimate, or reduce the sample rate of
  a set of traces stored in event RSEIS format
}
\usage{
DECIMATE.SEISN(TH, sel=1:length(TH$JSTR), dec=5 ,
 type="LP", proto="BU" , fl=2,  fh=10, RM=FALSE, zp=TRUE )
}
\arguments{

  \item{TH}{RSEIS list }
  \item{sel}{numeric,  which traces to select }
  \item{dec}{numeric, number of samples to skip }
  \item{type}{type of filter (see butfilt), or FALSE for no filter  }
    \item{proto}{filter proto type }
    \item{fl}{low pass frequency cut off }
       \item{fh}{high pass frequency cut off }
        \item{RM}{Remove mean value from trace, default=FALSE}
  \item{zp}{zero phase filter, default=TRUE}
}
\details{
  Reduces the number of samples by skipping every "dec"
  sample.

  To achieve smoothing prior to sampling,
  low pass filter may be applied to avoid spikes or other sampling issues.

  If type is FALSE, no filter is applied and samples are taken
  from the input.
  
}
\value{
  an RSEIS list.
}

\author{Jonathan M. Lees<jonathan.lees@unc.edu>}
\note{ The dt, n and t2 are modified in info.
}

\seealso{butfilt, downsample }
\examples{

data(GH)

dec = 250/50

#####  resample all traces by reducing from 250 to 50 samples/s
DH = DECIMATE.SEISN(GH, sel=1:length(GH$JSTR), dec=dec ,
 type="LP", proto="BU" , fl=2,  fh=50, RM=FALSE, zp=TRUE )

#####  compare

#####   times in
### starting second should be the same
GH$info$sec[1:5]  - DH$info$sec[1:5] 
####  number of samples should be reduced
cbind(GH$info$n[1:5] , DH$info$n[1:5]  )
### ending seconds should be close but not identical
cbind(GH$info$t2[1:5] , DH$info$t2[1:5]  )

cbind(GH$info$dt[1:5] , DH$info$dt[1:5]  )

cbind( sapply(GH$JSTR, 'length'), sapply(DH$JSTR, 'length') )



####  for visual comparison:
### par(mfrow=c(2,1) )
## g =  swig(GH, sel=which(GH$COMPS=="V" ), SHOWONLY=0 )
## d =  swig(DH, sel=which(DH$COMPS=="V" ), SHOWONLY=0 )

  }

  
\keyword{misc}
