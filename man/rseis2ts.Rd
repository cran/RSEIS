\name{rseis2ts}
\alias{rseis2ts}
\title{Convert RSEIS to TS
}
\description{Convert one trace from an RSEIS seismic list to a ts time-series object.
}
\usage{
rseis2ts(GH, sel = 1, notes = "")
}
\arguments{
  \item{GH}{List structure of seismic traces from RSEIS
}
  \item{sel}{numeric index of one trace.
}
  \item{notes}{character string of notes
}
}
\details{Function extracts one trace and associated information from an RSEIS structure and returns a ts, time-series, object.
}
\value{ts object
}

\author{
Jonathan M. Lees<jonathan.lees@unc.edu>
}

\examples{

data(GH)
H = rseis2ts(GH, 1, notes='Coso Trace 1')
plot(H)
title(main=attr(H, 'info')$notes)

}
\keyword{misc}
