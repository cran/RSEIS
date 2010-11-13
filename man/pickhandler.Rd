\name{pickhandler}
\alias{pickhandler}
\title{Handle Pick in RSEIS}
\description{
  Update the WPX (pick data frame) list with a new pick.
}
\usage{
pickhandler(i1 = 1, ppick = 0, kzap = "Y", err = NA, res=0, ycol =
rgb(0, 0, 1), pol=0, flg=0, onoff=1, NPX = 1, WPX = WPX, NH)
}
\arguments{
  \item{i1}{Index of trace}
  \item{ppick}{time for pick in seconds}
  \item{kzap}{character label of pick}
  \item{err}{error for pick}
  \item{res}{residual(or duration)}
  \item{ycol}{color for pick}
  \item{pol}{polarity of pick}
  \item{flg}{flag for pick}
  \item{onoff}{turn or off for pick}
  \item{NPX}{index of pick in WPX}
  \item{WPX}{Pick data frame}
  \item{NH}{List of traces}
}

\note{
  If WPX is missing, it is created.
  If NH is missing (no seismic traces) program returns NULL.
  }

\value{
 Returns WPX data frame with new pick added (or replaced).
}

\author{Jonathan M. Lees<jonathan.lees@unc.edu>}

\seealso{swig, YPIX, WPIX, NOPIX, REPIX, PickWin, pADDPIX, Ppic,
  POLSWITCH, Pup}

\keyword{misc}
