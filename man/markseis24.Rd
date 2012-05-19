\name{markseis24}
\Rdversion{1.1}
\alias{markseis24}
\title{Mark 24 hour seismic display
}
\description{Mark a 24 hour seismic display
}
\usage{
markseis24(pjj, pix = list(yr = 2009, jd = 1, hr = 0, mi = 0, sec = 0,
dur = 0), col = "red", LEGON = 3, BARON = TRUE, ARROWS = TRUE, lwd=1)
}
\arguments{
  \item{pjj}{Output information from plotseis24 (x,y, yr, jd)
}
\item{pix}{list:
  date list consisting of: yr, jd, hr, mi, sec, dur)
}
  \item{col}{Color, specified as color index, character string or rgb
  }
\item{LEGON}{
      plotting flag for legs: 0=no legs, 1=left leg, 2=right leg, 3=both legs(def
ault)
}
\item{BARON}{
  logical:plotting flag for bar

}
  \item{ARROWS}{logical: plot arrows FALSE=no arrows
  }
  \item{lwd}{numeric, graphical parameter, line width
  }

  
}
\details{
  the LEGON parameter controls the small marks at the ends: Either left(1)
  right(2) both(3) or no legs(0) are plotted.  window bars should wrap
  around the ends of the hour to the next hour below.
  The durations of the windows are supplied in seconds.  If no duration
  is supplied, it is set to 0.  If one duration is supplied it is copied
  to all other windows.
}
\value{Graphical Side effects
}

\author{
Jonathan M. Lees<jonathan.lees@unc.edu>
}


\seealso{winmark, getseis24,  plotseis24
}
\examples{

\dontrun{
##  the database DB was constructed with makeDB

usta = "CAL"
acomp = "V"

iday = 2

###   get one day's worth of data
JJ = getseis24(DB, 2009, iday, "CAL", "V", kind = 1)

###  plot day
pjj = plotseis24(JJ, dy=1/18, FIX=24, SCALE=0, FILT=list(ON=FALSE, fl=0.5 , fh=20.0, type="BP", proto="BU"), RCOLS=c(rgb(0.2, .2, 1), rgb(.2, .2, .2))  )
title(paste("CAL", "V"))

###   set up pix
WINS2 = win15
Apix = WINS2$hr[seq(from=1, to=length(WINS2$hr), by=2) ]
dur = (WINS2$hr[seq(from=2, to=length(WINS2$hr), by=2) ]-Apix)*3600

## dur = rep(0, times=length(Apix))

##  mark the 24 hour plot
markseis24(pjj, pix =list(yr=rep(pjj$yr, length(Apix)), jd=rep(pjj$jd, length(Apix)) , hr=Apix, mi=rep(0, length(Apix)), sec=rep(0, length(Apix)), dur=dur), col='red', ARROWS=FALSE )




}


}
\keyword{misc}
