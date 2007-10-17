\name{getvertsorder}
\alias{getvertsorder}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Get Order of Picks from P-wave Arrivals}
\description{
  Uses a Pickfile and the Waveform file,
  and creates a vector ordering the waveforms
  by P-wave arrival.
}
\usage{
getvertsorder(P, GU)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{P}{Pickfile Structure}
  \item{GU}{Waveform structure}
}
\details{
  Waveforms structure may already have pickfile,
  but this is overridden by input pickfile P.
}
\value{
  list:
  \item{sel}{index of traces in order of first P-wave arrival}
  \item{win}{vector, c(1,2), time window from the first arrival to the last}
}

\author{Jonathan M. Lees<jonathan.lees.edu>}

\seealso{PICK.GEN}
\examples{
data(GH)

vertord =  getvertsorder(GH$pickfile, GH)

PICK.GEN(GH,  sel=vertord$sel, WIN=vertord$win,   SHOWONLY=TRUE)


}

\keyword{misc}
