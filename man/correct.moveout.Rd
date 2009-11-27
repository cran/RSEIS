\name{correct.moveout}
\alias{correct.moveout}
\title{Moveout Correction
}
\description{Shift traces accoring to given moveout times
}
\usage{
correct.moveout(GH, sel = 1, tims = 0)
}
\arguments{
  \item{GH}{RSEIS structure list
}
  \item{sel}{index of which traces to be shifted 
}
  \item{tims}{time shifts for each trace
}
}
\details{
  Each trace listed in sel gets shifted forward or backward according to
  time in tims.  This is useful for shifting traces according to a given
  moveout curve.

  
}
\value{RSEIS list structure returned with adjusted traces
}

\author{
Jonathan M. Lees<jonathan.lees@unc.edu>
}

\keyword{misc}
