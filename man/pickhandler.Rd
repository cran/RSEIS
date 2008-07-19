\name{pickhandler}
\alias{pickhandler}
\title{Handle Picks in PICK.GEN}
\description{
  Update the WPX list with a new pick
}
\usage{
pickhandler(i1 = 1, ppick = 0, kzap = "Y", err = NA, ycol = rgb(0, 0, 1), NPX = 1, WPX = WPX, NH)
}
\arguments{
  \item{i1}{Index of trace}
  \item{ppick}{time for pick in seconds}
  \item{kzap}{character label of pick}
  \item{err}{error for pick}
  \item{ycol}{color for pick}
  \item{NPX}{index of pick in WPX}
  \item{WPX}{Pick data frame}
  \item{NH}{List of traces}
}

\value{
 returns WPX data frame with new pick added (or replaced) 
}

\author{Jonathan M. Lees<jonathan.lees@unc.edu>}

\seealso{PICK.GEN}

\keyword{misc}
