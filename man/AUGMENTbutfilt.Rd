\name{AUGMENTbutfilt}
\alias{AUGMENTbutfilt}
\title{Butterworth filter with Augmentation
}
\description{
 Design and apply butterworth low/high/band pass filters with augmentation of
 the signal on either end to suppress edge effects.
}
\usage{
AUGMENTbutfilt(a, fl = 0, fh = 0.5, deltat = 1, type = "BP",
proto = "BU", npoles = 5, chebstop = 30, trbndw = 0.3,
RM = FALSE, zp = TRUE, pct = 0.1)
}

\arguments{
  \item{a}{vector signal}
  \item{fl}{low frequency cut-off, default=0 }
  \item{fh}{high frequency cut-off, DEFAULT= (1/2dt) }
  \item{deltat}{sample rate, s, deFAULT=1}
  \item{type}{type of filter, one of c("LP",  "HP","BP" ,"BR" ), DEFAULT="BP" }
  \item{proto}{prototype, c("BU", "BE" , "C1" ,"C2"), DEFAULT="BU"  }
  \item{npoles}{number of poles or order, DEFAULT=5}
  \item{chebstop}{Chebyshev stop band attenuation, DEFAULT=30.0}
  \item{trbndw}{Chebyshev transition bandwidth, DEFAULT=0.3}
  \item{RM}{Remove mean value from trace, default=FALSE}
  \item{zp}{zero phase filter, default=TRUE}
  \item{pct}{Percent augmentation applied to each side, default=0.1}
  
}
\details{
  Creation of butfilt is a described by the following arguments:
  \describe{
    \item{LP}{low pass  }
    \item{HP}{high pass }
    \item{BP}{band pass }
    \item{BR}{band reject }
    \item{BU}{Butterworth }
    \item{BE}{Bessel }
    \item{C1}{Chebyshev type 1 }
    \item{C2}{Chebyshev type 2 }
  }

  Arguments chebstop , trbndw  are ignored for non-chebyshev filters.
  LP and HP filters are seet by specifying fl for HP filters and fh for
  LP filters, the other argumentin each case is ignored.

  Mean values should be removed prior to calling this function, and then set RM=FALSE.
  This is true especially if tapering is applied prior to filtering.

  Zero phase filter is achived by running filter back and forth.  Otherwise
  a single pass is returned.  This should be equivalent to package signal filtfilt (from MATLAB).

  Augmentation involves copying the first and last percent of the signal,
  reversiing the time and adding to the signal on each end.  This is then filtered,
  and removed after filter is complete.  It is assumed that the important part of the signal is in
  the center of the time series and the edges are less critical.  Then the augmented part has the same statistical
  content as the edges of the signal (presumably noise) and will not affect the filtered signal considerably.
  This is then thrown away prior to return.

}
\value{
Filtered time series with the augmentation removed after filter.
}

\author{Jonathan M. Lees<jonathan.lees.edu>}

\seealso{butfilt}

\examples{
data(CE1)

ts1  <-  CE1$y
zz  <-  AUGMENTbutfilt(ts1, fl=1, fh=15,  deltat=CE1$dt, type="LP" ,  proto="BU",
npoles=5 )


##############    second example with plotting


data(KH, package ='RSEIS' )
w = KH$JSTR[[1]]
dt = KH$dt[1]

x = seq(from=0, by=dt, length=length(w));
plot(x,w, type='l')

par(mfrow=c(2,1) )
        
i=1
       fl = 1/50
fh= 1/2
       ftype = 'BP'
       ##########  normal band pass filter
        
 zz = butfilt(w, fl, fh,  dt, ftype ,  "BU")
     f.stamp =   filterstamp(fl=fl, fh=fh, type=ftype)

plot(x, zz, type='l', xlab='s', ylab='amp', main= f.stamp)
title(sub='butfilt')
       
       ####  
   zz1 = AUGMENTbutfilt(w, fl, fh,  dt,    type=ftype ,  proto="BU", zp=TRUE, pct=0.2 )
     f.stamp =   filterstamp(fl=fl, fh=fh, type=ftype)
plot(x, zz1, type='l', xlab='s', ylab='amp', main= f.stamp)
title(sub='AUGMENTbutfilt')






}

\keyword{misc}
