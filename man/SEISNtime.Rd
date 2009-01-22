\name{SEISNtime}
\alias{SEISNtime}
\title{Minimum time in an RSEIS list}
\description{
  Return date/time of trace with earliest date/time.
}
\usage{
SEISNtime(GH)
}
\arguments{
  \item{GH}{RSEIS seismic list}
}

\value{
  \item{yr}{year}
  \item{jd}{julian day}
  \item{hr}{hour}
  \item{mi}{minute}
  \item{sec}{second}
  \item{w1}{which one, index to GH}
    
}

\author{Jonathan M. Lees<jonathan.lees@unc.edu>}

\examples{


data(GH)
SEISNtime(GH)


}
\keyword{misc}
