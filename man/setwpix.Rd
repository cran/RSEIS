\name{setwpix}
\alias{setwpix}
\title{Set Window Pix for swig}
\description{
  Create list of windows picks suitable for plotting in
  swig.
}
\usage{
setwpix(phase = NULL, col = NULL, yr = NULL, jd = NULL, hr = NULL, mi = NULL, sec = NULL, dur = NULL, name = NULL, comp = NULL, dispcomp = NULL)
}
\arguments{
  \item{phase}{phase name}
  \item{col}{color for plotting}
  \item{yr}{year}
  \item{jd}{julian day}
  \item{hr}{hour}
  \item{mi}{minute}
  \item{sec}{second}
  \item{dur}{duration}
  \item{name}{name of station}
  \item{comp}{component}
  \item{dispcomp}{display on which component }
}
\details{
  Some phases should be displayed on only certain components of a station.
}
\value{
  list of window picks
}

\author{Jonathan M. Lees<jonathan.lees@unc.edu>}

\seealso{swig}
\examples{


data(KH)

orgtim=c( 2005,214,7,1,10.7313152551651 )
tims=c( 0,46.7119336128235,102.438451051712,113.092049956322,123.540771961212 )
psecs = NULL
nam = NULL

aphases =NULL
sta ="9024"


for(j in 1:length(tims))
{
psecs = c(psecs, tims[j]+orgtim[5])
nam = c(nam, sta)
aphases = c(aphases, paste(sep="", "K", j) )
}


pp = setwpix(phase=aphases , col="blue", yr=orgtim[1], jd=orgtim[2],
hr=orgtim[3],  mi=orgtim[4], sec=psecs, dur=0, name=nam , comp="V")



W = secdifL(KH$info, pp)

win = c(min(W)-5, max(W)+5    )  
 swig(KH, APIX=pp, WIN=win , SHOWONLY=TRUE)


}
\keyword{misc}
